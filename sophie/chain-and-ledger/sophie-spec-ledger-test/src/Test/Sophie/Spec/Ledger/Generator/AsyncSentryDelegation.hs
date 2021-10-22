{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Sophie.Spec.Ledger.Generator.VestedSealDelegation (
genDCert, 
CertCred,
mkOCert,
mkAcert,
genStakePool, 
pickStakeKey,
)
where

import Bcc.Ledger.Address (mkRwdAcnt)
import Bcc.Ledger.BaseTypes (UnitInterval)
import Bcc.Ledger.Coin (DeltaCoin (..), toDeltaCoin)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era (Crypto, Era, ValidateScript (..))
import Bcc.Ledger.Keys
  ( coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
    vKey,
  )
import Bcc.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import Control.Monad (replicateM)
import Control.SetAlgebra (dom, domain, eval, (∈), (∉))
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, findWithDefault, fromList, keys, lookup, size)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set ((\\))
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Numeric.Natural (Natural)
import Sophie.Spec.Ledger.API
  ( AccountState (..),
    Coin (..),
    Credential (..),
    DCert (..),
    DPState (..),
    DState (..),
    DelegCert (..),
    Delegation (..),
    GenDelegPair (..),
    GenDelegs (..),
    GenesisDelegCert (..),
    VestedDelegPair (..),
    VestedDelegs (..),
    VestedDelegCert (..),
    KeyHash,
    KeyPair,
    KeyPairs,
    KeyRole (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    Network (..),
    PState (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    StrictMaybe (..),
    VKey,
  )
import qualified Sophie.Spec.Ledger.HardForks as HardForks
import Sophie.Spec.Ledger.LedgerState (availableAfterMIR)
import Sophie.Spec.Ledger.PParams (ProtVer)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Sophie.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Sophie.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    KeySpace (..),
    genInteger,
    genWord64,
    toCred,
    tooLateInEpoch,
  )
import Test.Sophie.Spec.Ledger.Generator.EraGen (EraGen (..))
import Test.Sophie.Spec.Ledger.SentryUtils(testGlobals)
-- ======================================================

data CertCred era
  = CoreKeyCred [GenesisKeyPair (Crypto era)]
  | VestedKeyCred [VestedKeyPair (Crypto era)]
  | StakeCred (KeyPair 'Staking (Crypto era))
  | PoolCred (KeyPair 'StakePool (Crypto era))
  | ScriptCred (Core.Script era, Core.Script era)
  | DelegateCred [KeyPair 'GenesisDelegate (Crypto era)]
  | VestedDelegateCred [KeyPair 'VestedDelegate (Crypto era)]
  | NoCred

deriving instance (Era era, Show (Core.Script era)) => Show (CertCred era)

-- | Occasionally generate a valid certificate
--
-- Returning `Nothing` indicates a failure to generate a value, usually due to lack of
-- available values from the pre-populated (e.g. key) spaces.
-- A `Just` represents a successfully generated value.
--
-- Different generators return witnesses that are either vested, vested, or regular keys.
--
-- Note: we register keys and pools more often than deregistering/retiring them,
-- and we generate more delegations than registrations of keys/pools.
genDCert ::
  forall era.
  EraGen era =>
  Constants ->
  KeySpace era ->
  Core.PParams era ->
  AccountState ->
  DPState (Crypto era) ->
  SlotNo ->
  Gen (Maybe (DCert (Crypto era), CertCred era))
genDCert
  c@( Constants
        { frequencyRegKeyCert,
          frequencyRegPoolCert,
          frequencyDelegationCert,
          frequencyGenesisDelegationCert,
          frequencyVestedDelegationCert, 
          frequencyDeRegKeyCert,
          frequencyRetirePoolCert,
          frequencyMIRCert
        }
      )
  KeySpace_
    { ksCoreNodes,
      ksCoreVested,
      ksKeyPairs,
      ksMSigScripts,
      ksStakePools,
      ksGenesisDelegates,
      ksIndexedGenDelegates,
      ksVestedDelegates,
    }
  pparams
  accountState
  dpState
  slot =
    QC.frequency
      [ (frequencyRegKeyCert, genRegKeyCert c ksKeyPairs ksMSigScripts dState),
        (frequencyRegPoolCert, genRegPool ksStakePools ksKeyPairs (getField @"_minPoolCost" pparams)),
        (frequencyDelegationCert, genDelegation c ksKeyPairs ksMSigScripts dpState),
        ( frequencyGenesisDelegationCert,
          genGenesisDelegation ksCoreNodes ksGenesisDelegates dpState
        ),
        (frequencyVestedDelegationCert,
          genVestedDelegation ksCoreVested ksVestedDelegates dpState
        ),
        (frequencyDeRegKeyCert, genDeRegKeyCert c ksKeyPairs ksMSigScripts dState),
        (frequencyRetirePoolCert, genRetirePool pparams ksStakePools pState slot),
        ( frequencyMIRCert,
          genInstantaneousRewards
            slot
            ksIndexedGenDelegates
            pparams
            accountState
            dState
        )
      ]
    where
      dState = _dstate dpState
      pState = _pstate dpState

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: KeyPairs crypto -> Gen (VKey 'Staking crypto)
pickStakeKey keys = vKey . snd <$> QC.elements keys

--make operational cert for eligible including vested
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
-- | make tx for creation of new vested delegates
vestedCoins ::
  (Era era) =>
  Ledger.TxId (Crypto era) ->
  [Core.TxOut era] ->
  UTxO era
vestedCoins vestedTxId outs =
  UTxO $
    Map.fromList [(TxIn vestedTxId idx, out) | (idx, out) <- zip [0 ..] outs]

-- | check for instantaneous rewards from Utxow.hs
  let vestedDelegates =
        Set.fromList $
          asWitness . vestedDelegKeyHash
            <$> Map.elems vestedMapping
      (WitHashes khAsSet) = witsKeyHashes
      genSig = eval (vestedDelegates ∩ khAsSet)
      mirCerts =
        StrictSeq.forceToStrict
          . Seq.filter isInstantaneousRewards
          . StrictSeq.fromStrict
          $ getField @"certs" txbody
      VestedDelegs vestedMapping = vestedDelegs

-- | cheaper to request all at once instead of referencing multiples
genStakePool ::
  forall crypto.
  (CC.Crypto crypto) =>
  -- | Available keys for stake pool registration
  [AllIssuerKeys crypto 'StakePool] ->
  -- | KeyPairs containing staking keys to act as owners/reward account
  KeyPairs crypto ->
  -- | Minimum pool cost Protocol Param
  Coin ->
  Gen (PoolParams crypto, KeyPair 'StakePool crypto)
genStakePool poolKeys skeys (Coin minPoolCost) =
  mkPoolParams
    <$> QC.elements poolKeys
    <*> ( Coin -- pledge
            <$> QC.frequency
              [ (1, genInteger 1 100),
                (5, pure 0)
              ]
        )
    <*> (Coin <$> genInteger minPoolCost (minPoolCost + 50)) -- cost
    <*> (fromInteger <$> QC.choose (0, 100) :: Gen Natural)
    <*> getAnyStakeKey skeys
  where
    getAnyStakeKey :: KeyPairs crypto -> Gen (VKey 'Staking crypto)
    getAnyStakeKey keys = vKey . snd <$> QC.elements keys
    mkPoolParams allPoolKeys pledge cost marginPercent acntKey =
      let interval = unsafeBoundRational $ fromIntegral marginPercent % 100
          pps =
            PoolParams
              (hashKey . vKey . cold $ allPoolKeys)
              (hashVerKeyVRF . snd . vrf $ allPoolKeys)
              pledge
              cost
              interval
              (RewardAcnt Testnet $ KeyHashObj $ hashKey acntKey)
              Set.empty
              StrictSeq.empty
              SNothing
       in (pps, cold allPoolKeys)

genVestedDelegation ::
  (Era era) =>
  -- | Core nodes
  [(VestedKeyPair (Crypto era), AllIssuerKeys (Crypto era) 'VestedDelegate)] ->
  -- | All potential vested delegate keys
  [AllIssuerKeys (Crypto era) 'VestedDelegate] ->
  DPState (Crypto era) ->
  Gen (Maybe (DCert (Crypto era), CertCred era))
genVestedDelegation coreVested delegateKeys dpState =
  if null vestedDelegators || null availableDelegatees
    then pure Nothing
    else do
      gk <- QC.elements vestedDelegators
      AllIssuerKeys {cold, vrf} <- QC.elements availableDelegatees
      case Map.lookup (hashVKey gk) vestedDelegs_ of
        Nothing -> pure Nothing
        Just _ -> return $ mkCert gk cold (snd vrf)
  where
    allDelegateKeys = (snd <$> coreVested) <> delegateKeys
    hashVKey = hashKey . vKey
    mkCert gkey key vrf =
      Just
        ( DCertVested
            ( VestedDelegCert
                (hashVKey gkey)
                (hashVKey key)
                (hashVerKeyVRF vrf)
            ),
          CoreKeyCred [gkey]
        )
    (VestedDelegs vestedDelegs_) = _vesteddelegs $ _dstate dpState
    vestedDelegator k = eval (k ∈ dom vestedDelegs_)
    vestedDelegators = filter (vestedDelegator . hashVKey) (fst <$> coreVested)
    notActiveDelegatee k = not (coerceKeyRole k `List.elem` fmap VestedDelegKeyHash (Map.elems vestedDelegs_))
    fGenDelegs = _fGenDelegs $ _dstate dpState
    notFutureDelegatee k = not (coerceKeyRole k `List.elem` fmap VestedDelegKeyHash (Map.elems fVestedDelegs))
    notDelegatee k = notActiveDelegatee k && notFutureDelegatee k
    availableDelegatees = filter (notDelegatee . hashVKey . cold) allDelegateKeys
