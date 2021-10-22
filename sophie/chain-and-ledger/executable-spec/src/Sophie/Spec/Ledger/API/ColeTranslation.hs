{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Sophie.Spec.Ledger.API.ColeTranslation
  ( mkInitialSophieLedgerView,
    translateToSophieLedgerState,

    -- * Exported for testing purposes
    translateCompactTxOutColeToSophie,
    translateTxIdColeToSophie,
  )
where

import qualified Bcc.Chain.Block as Cole
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Bcc.Crypto.Hash as Crypto
import qualified Bcc.Crypto.Hashing as Hashing
import Bcc.Ledger.Coin (CompactForm (CompactCoin))
import qualified Bcc.Ledger.Crypto as CC
import Bcc.Ledger.SafeHash (unsafeMakeSafeHash)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot
import Bcc.Ledger.Val ((<->))
import qualified Data.ByteString.Short as SBS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Sophie.Spec.Ledger.API.Protocol
import Sophie.Spec.Ledger.API.Types
import Sophie.Spec.Ledger.CompactAddr (CompactAddr (UnsafeCompactAddr))
import Sophie.Spec.Ledger.EpochBoundary
import Sophie.Spec.Ledger.STS.Chain (pparamsToChainChecksData)

-- | We use the same hashing algorithm so we can unwrap and rewrap the bytes.
-- We don't care about the type that is hashed, which will differ going from
-- Cole to Sophie, we just use the hashes as IDs.
translateTxIdColeToSophie ::
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  Cole.TxId ->
  TxId c
translateTxIdColeToSophie =
  TxId . unsafeMakeSafeHash . hashFromShortBytesE . Hashing.abstractHashToShort

hashFromShortBytesE ::
  forall h a.
  (Crypto.HashAlgorithm h, HasCallStack) =>
  SBS.ShortByteString ->
  Crypto.Hash h a
hashFromShortBytesE sbs = fromMaybe (error msg) $ Crypto.hashFromBytesShort sbs
  where
    msg =
      "hashFromBytesShort called with ShortByteString of the wrong length: "
        <> show sbs

translateCompactTxOutColeToSophie :: Cole.CompactTxOut -> TxOut (SophieEra c)
translateCompactTxOutColeToSophie (Cole.CompactTxOut compactAddr amount) =
  TxOutCompact
    (UnsafeCompactAddr (Cole.unsafeGetCompactAddress compactAddr))
    (CompactCoin (Cole.unsafeGetEntropic amount))

translateCompactTxInColeToSophie ::
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  Cole.CompactTxIn ->
  TxIn c
translateCompactTxInColeToSophie (Cole.CompactTxInUtxo compactTxId idx) =
  TxInCompact
    (translateTxIdColeToSophie (Cole.fromCompactTxId compactTxId))
    (fromIntegral idx)

translateUTxOColeToSophie ::
  forall c.
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  Cole.UTxO ->
  UTxO (SophieEra c)
translateUTxOColeToSophie (Cole.UTxO utxoCole) =
  UTxO $
    Map.fromList
      [ (txInSophie, txOutSophie)
        | (txInCole, txOutCole) <- Map.toList utxoCole,
          let txInSophie = translateCompactTxInColeToSophie txInCole
              txOutSophie = translateCompactTxOutColeToSophie txOutCole
      ]

translateToSophieLedgerState ::
  forall c.
  (CC.Crypto c, CC.ADDRHASH c ~ Crypto.Blake2b_224) =>
  SophieGenesis (SophieEra c) ->
  EpochNo ->
  Cole.ChainValidationState ->
  NewEpochState (SophieEra c)
translateToSophieLedgerState genesisSophie epochNo cvs =
  NewEpochState
    { nesEL = epochNo,
      nesBprev = BlocksMade Map.empty,
      nesBcur = BlocksMade Map.empty,
      nesEs = epochState,
      nesRu = SNothing,
      nesPd = PoolDistr Map.empty
    }
  where
    pparams :: PParams (SophieEra c)
    pparams = sgProtocolParams genesisSophie

    -- NOTE: we ignore the Cole delegation map because the genesis and
    -- delegation verification keys are hashed using a different hashing
    -- scheme. This means we can't simply convert them, as Cole nowhere stores
    -- the original verification keys.
    --
    -- Fortunately, no Cole genesis delegations have happened yet, and if
    -- they did, we would be aware of them before the hard fork, as we
    -- instigate the hard fork. We just have to make sure that the hard-coded
    -- Sophie genesis contains the same genesis and delegation verification
    -- keys, but hashed with the right algorithm.
    genDelegs :: GenDelegs c
    genDelegs = GenDelegs $ sgGenDelegs genesisSophie

    vestedDelegs :: VestedDelegs c
    vestedDelegs = VestedDelegs $ sgVestedDelegs genesisSophie

    reserves :: Coin
    reserves =
      word64ToCoin (sgMaxEntropicSupply genesisSophie) <-> balance utxoSophie

    epochState :: EpochState (SophieEra c)
    epochState =
      EpochState
        { esAccountState = AccountState (Coin 0) reserves,
          esSnapshots = emptySnapShots,
          esLState = ledgerState,
          esPrevPp = pparams,
          esPp = pparams,
          esNonMyopic = def
        }

    utxoCole :: Cole.UTxO
    utxoCole = Cole.cvsUtxo cvs

    utxoSophie :: UTxO (SophieEra c)
    utxoSophie = translateUTxOColeToSophie utxoCole

    ledgerState :: LedgerState (SophieEra c)
    ledgerState =
      LedgerState
        { _utxoState =
            UTxOState
              { _utxo = utxoSophie,
                _deposited = Coin 0,
                _fees = Coin 0,
                _ppups = def
              },
          _delegationState =
            DPState
              { _dstate = (def {_genDelegs = genDelegs}{_vestedDelegs = vestedDelegs}),
                _pstate = def
              }
        }

-- | We construct a 'LedgerView' using the Sophie genesis config in the same
-- way as 'translateToSophieLedgerState'.
mkInitialSophieLedgerView ::
  forall c.
  SophieGenesis (SophieEra c) ->
  LedgerView c
mkInitialSophieLedgerView genesisSophie =
  LedgerView
    { lvD = _d . sgProtocolParams $ genesisSophie,
      lvExtraEntropy = _extraEntropy . sgProtocolParams $ genesisSophie,
      lvPoolDistr = PoolDistr Map.empty,
      lvGenDelegs = GenDelegs $ sgGenDelegs genesisSophie,
      lvChainChecks = pparamsToChainChecksData . sgProtocolParams $ genesisSophie
    }
