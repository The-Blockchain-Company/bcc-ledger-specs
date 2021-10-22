{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sophie.Spec.Ledger.VestedSeal
  ( VestedSealEnv (..),
    sealFactor
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import qualified Bcc.Crypto.Hash.Class as Crypto
import Bcc.Crypto.KES.Class (totalPeriodsKES)
import Bcc.Ledger.Address
import Bcc.Ledger.BaseTypes
import Bcc.Ledger.Coin
import Bcc.Ledger.Crypto (HASH, KES)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era
import Bcc.Ledger.Hashes (EraIndependentTxBody)
import Bcc.Ledger.Keys
import Bcc.Ledger.SafeHash (unsafeMakeSafeHash)
import Bcc.Ledger.Serialization
  ( decodeRecordNamed,
    mapFromCBOR,
    mapToCBOR,
    utcTimeFromCBOR,
    utcTimeToCBOR,
  )
import Bcc.Ledger.Sophie.Constraints (UsesTxOut (..))
import qualified Bcc.Ledger.Val as Val
import Bcc.Prelude (forceElemsToWHNF)
import Bcc.Slotting.EpochInfo
import Bcc.Slotting.Slot (EpochSize (..))
import Bcc.Slotting.Time (SystemStart (SystemStart))
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime (..))
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import NoThunks.Class (NoThunks (..))
import Sophie.Spec.Ledger.PParams
import Sophie.Spec.Ledger.StabilityWindow
import Sophie.Spec.Ledger.TxBody (PoolParams (..), TxId (..), TxIn (..))
import Sophie.Spec.Ledger.UTxO

import Bcc.Crypto.Hash
  ( Blake2b_256,
    Hash,
    HashAlgorithm,
    hashToBytes,
    hashWithSerialiser,
  )
import Bcc.Crypto.KES
  ( KESAlgorithm,
    SignKeyKES,
    VerKeyKES,
    deriveVerKeyKES,
    genKeyKES,
  )
import Bcc.Crypto.KES.Class (ContextKES)
import Bcc.Crypto.Seed (Seed, mkSeedFromBytes)
import Bcc.Crypto.VRF
  ( CertifiedVRF,
    SignKeyVRF,
    VRFAlgorithm (..),
    VerKeyVRF,
    certifiedOutput,
    deriveVerKeyVRF,
    evalCertified,
    genKeyVRF,
  )
import qualified Bcc.Crypto.VRF as VRF
import Bcc.Ledger.Address (Addr, pattern Addr)
import Bcc.Ledger.BaseTypes
  ( BoundedRational (..),
    Globals (..),
    Network (..),
    Nonce,
    SophieBase,
    epochInfo,
    mkActiveSlotCoeff,
    mkNonceFromOutputVRF,
  )
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Credential (Credential (..), StakeReference (..))
import Bcc.Ledger.Crypto (DSIGN)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era (Crypto (..))
import qualified Bcc.Ledger.Era as Era
import Bcc.Ledger.Keys
  ( KeyPair,
    KeyRole (..),
    VKey (..),
    hashKey,
    updateKES,
    vKey,
    pattern KeyPair,
  )
import Bcc.Ledger.Sophie.Constraints
import Bcc.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import Bcc.Ledger.Seal (SealNo, sealEpochInfoFirst, sealSlotInfoEpoch)
import Bcc.Prelude (Coercible, asks)
import Bcc.Protocol.TOptimum.BHeader (BHBody (..), bhbody)
import Bcc.Protocol.TOptimum.OCert (KESPeriod (..))
import Bcc.Slotting.EpochInfo
  ( epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
    fixedEpochInfo,
  )
import Bcc.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace
  ( applySTSTest,
    checkTrace,
    (.-),
    (.->),
  )
import Data.Coerce (coerce)
import Data.Default.Class (Default)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Data.Typeable (Proxy (Proxy))
import Data.Word (Word64)
import GHC.Stack
import Sophie.Spec.Ledger.API
  ( ApplyBlock,
    GetLedgerView,
    PParams,
  )
import Sophie.Spec.Ledger.BlockChain (Block, TxSeq, bheader)
import Sophie.Spec.Ledger.PParams (PParamsUpdate)
import Sophie.Spec.Ledger.Tx (Tx, TxOut, WitnessSet)

-- | Vested Seal Configuration.
--
-- This allows us to configure the initial SealConstruct 
-- in order to test Optimum in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data VestedSealEnv crypto = VestedSealEnv
  {
    txCounts :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | Stake-holding key hash credentials and the pools to delegate that stake
    -- to. We require the raw staking key hash in order to:
    --ne    -- - Avoid pointer addresses, which would be tricky when there's no slot or
    --   transaction to point to.
    -- - Avoid script credentials.
    sgsStake :: !(Map (KeyHash 'Staking crypto) (KeyHash 'StakePool crypto))

  }
  deriving stock (Eq, Show, Generic)

instance NoThunks (SophieGenesisStaking crypto)

instance CC.Crypto crypto => ToCBOR (SophieGenesisStaking crypto) where
  toCBOR (SophieGenesisStaking pools stake) =
    encodeListLen 2 <> mapToCBOR pools <> mapToCBOR stake
instance CC.Crypto crypto => FromCBOR (SophieGenesisStaking crypto) where
  fromCBOR = do
    decodeRecordNamed "SophieGenesisStaking" (const 2) $ do
      pools <- mapFromCBOR
      stake <- mapFromCBOR
      pure $ SophieGenesisStaking pools stake

sealFactor :: Float
sealFactor = 2.0


data sealConstruct crypto = sealConstruct
  
  
  { numTx :: Map (TxIn crypto)

  }


vestedSealKeyGen ::
  (Era era) =>
  Ledger.TxId (Crypto era) ->
  [Core.TxOut era] ->
  UTxO era
vestedSealKeyGen vestedTxId outs =
  UTxO $
    Map.fromList [(TxIn vestedTxId idx, out) | (idx, out) <- zip [0 ..] outs]

mkOCert ::
  forall crypto r.
  (CC.Crypto crypto, Signable (DSIGN crypto) (OCertSignable crypto)) =>
  AllIssuerKeys crypto r ->
  Word64 ->
  KESPeriod ->
  OCert crypto
mkOCert pkeys n c0 =
  let (_, (_, vKeyHot)) = head $ hot pkeys
      KeyPair _vKeyCold sKeyCold = cold pkeys
   in OCert
        vKeyHot
        n
        c0
        (signedDSIGN @crypto sKeyCold (OCertSignable vKeyHot n c0))
-- | Future Vested Delegates 
data FutureVestedDeleg crypto = FutureVestedDeleg
  { fVestedDelegSlot :: !SlotNo,
    fVestedDelegVestedKeyHash :: !(KeyHash 'Vested crypto)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (FutureVestedDeleg crypto)

instance NFData (FutureVestedDeleg crypto)

instance CC.Crypto crypto => ToCBOR (FutureVestedDeleg crypto) where
  toCBOR (FutureVestedDeleg a b) =
    encodeListLen 2 <> toCBOR a <> toCBOR b

instance CC.Crypto crypto => FromCBOR (FutureVestedDeleg crypto) where
  fromCBOR =
    decodeRecordNamed "FutureVestedDeleg" (const 2) $
      FutureVestedDeleg <$> fromCBOR <*> fromCBOR

-- | Aesity 
-- | Eternity
-- | Goodness
-- | Graciousness
-- | Holiness
-- | Immanence 
-- | Immutability 
-- | Impassibility 

data FutureVestedDeleg crypto = FutureVestedDeleg
  { fVestedDelegSlot :: !SlotNo,
    fVestedDelegGenKeyHash :: !(KeyHash 'Vested crypto)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (FutureVestedDeleg crypto)

instance NFData (FutureVestedDeleg crypto)

instance CC.Crypto crypto => ToCBOR (FutureVestedDeleg crypto) where
  toCBOR (FutureVestedDeleg a b) =
    encodeListLen 2 <> toCBOR a <> toCBOR b

instance CC.Crypto crypto => FromCBOR (FutureVestedDeleg crypto) where
  fromCBOR =
    decodeRecordNamed "FutureVestedDeleg" (const 2) $
      FutureVestedDeleg <$> fromCBOR <*> fromCBOR

  DCertVested (VestedDelegCert akh vkh vrf) -> do
      sp <- liftSTS $ asks stabilityWindow
      -- note that pattern match is used instead of vestedDeleg, as in the spec
      let s' = slot +* Duration sp
          (VestedDelegs vestedDelegs) = _vestedDelegs ds

      -- akh ∈ dom vestedDelegs ?! VestedKeyNotInMappingDELEG akh
      (case Map.lookup akh vestedDelegs of Just _ -> True; Nothing -> False) ?! VestedKeyNotInMappingDELEG akh

      let cod =
            range $
              Map.filterWithKey (\a _ -> a /= akh) vestedDelegs
          fod =
            range $
              Map.filterWithKey (\(FutureVestedDeleg _ a) _ -> a /= akh) (_fVestedDelegs ds)
          currentOtherColdKeyHashes = Set.map vestedDelegKeyHash cod
          currentOtherVrfKeyHashes = Set.map vestedDelegVrfHash cod
          futureOtherColdKeyHashes = Set.map genVestedKeyHash fod
          futureOtherVrfKeyHashes = Set.map genVestedVrfHash fod

      eval (vkh ∉ (currentOtherColdKeyHashes ∪ futureOtherColdKeyHashes))
        ?! DuplicateVestedDelegateDELEG vkh
      eval (vrf ∉ (currentOtherVrfKeyHashes ∪ futureOtherVrfKeyHashes))
        ?! DuplicateVestedVRFDELEG vrf

      pure $
        ds
          { _fVestedDelegs = eval (_fVestedDelegs ds ⨃ singleton (FutureVestedDeleg s' akh) (VestedDelegPair vkh vrf))
          }