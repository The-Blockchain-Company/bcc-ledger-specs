{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is a stub example of a Sophie era, designed for testing,
-- prototyping, and demo purposes.
module Bcc.Ledger.Example where

import Bcc.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import Bcc.Ledger.Coin (Coin)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (Era (Crypto), SupportsSegWit (..), ValidateScript (..))
import Bcc.Ledger.Hashes (EraIndependentAuxiliaryData)
import Bcc.Ledger.SafeHash (makeHashWithExplicitProxys)
import Bcc.Ledger.Sophie.Constraints (UsesPParams (..), UsesTxBody, UsesTxOut (..), UsesValue)
import Bcc.Ledger.Val (Val ((<->)))
import Bcc.Protocol.TOptimum.Rules.OCert (OCERT)
import Bcc.Protocol.TOptimum.Rules.Overlay (OVERLAY)
import qualified Data.ByteString as BS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Sophie.Spec.Ledger.API
  ( AccountState (AccountState),
    ApplyBlock,
    ApplyTx,
    CanStartFromGenesis (..),
    Coin (Coin),
    DPState (DPState),
    DState (_genDelegs),
    EpochState (EpochState),
    GenDelegs (GenDelegs),
    GetLedgerView,
    LedgerState (LedgerState),
    NewEpochState (NewEpochState),
    PoolDistr (PoolDistr),
    OptimumCrypto,
    SophieBasedEra,
    SophieGenesis (sgGenDelegs, sgMaxEntropicSupply, sgProtocolParams),
    StrictMaybe (SNothing),
    TxOut (..),
    UTxOState (UTxOState),
    balance,
    genesisUTxO,
    word64ToCoin,
  )
import qualified Sophie.Spec.Ledger.BlockChain as Sophie
  ( TxSeq (..),
    bbHash,
    txSeqTxns,
  )
import Sophie.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import Sophie.Spec.Ledger.Metadata (Metadata (Metadata), validMetadatum)
import Sophie.Spec.Ledger.PParams ()
import Sophie.Spec.Ledger.PParams as Sophie
import qualified Sophie.Spec.Ledger.PParams as SPP
import Sophie.Spec.Ledger.STS.Bbody (BBODY)
import Sophie.Spec.Ledger.STS.Deleg (DELEG)
import Sophie.Spec.Ledger.STS.Delegs (DELEGS)
import Sophie.Spec.Ledger.STS.Delpl (DELPL)
import Sophie.Spec.Ledger.STS.Epoch (EPOCH)
import Sophie.Spec.Ledger.STS.Ledger (LEDGER)
import Sophie.Spec.Ledger.STS.Ledgers (LEDGERS)
import Sophie.Spec.Ledger.STS.Mir (MIR)
import Sophie.Spec.Ledger.STS.NewEpoch (NEWEPOCH)
import Sophie.Spec.Ledger.STS.Newpp (NEWPP)
import Sophie.Spec.Ledger.STS.Pool (POOL)
import Sophie.Spec.Ledger.STS.PoolReap (POOLREAP)
import Sophie.Spec.Ledger.STS.Ppup (PPUP)
import Sophie.Spec.Ledger.STS.Rupd (RUPD)
import Sophie.Spec.Ledger.STS.Snap (SNAP)
import Sophie.Spec.Ledger.STS.Tick (TICK, TICKF)
import Sophie.Spec.Ledger.STS.Tickn (TICKN)
import Sophie.Spec.Ledger.STS.Upec (UPEC)
import Sophie.Spec.Ledger.STS.Utxo (UTXO)
import Sophie.Spec.Ledger.STS.Utxow (UTXOW)
import Sophie.Spec.Ledger.Scripts (MultiSig)
import Sophie.Spec.Ledger.Tx
  ( Tx,
    WitnessSet,
    validateNativeMultiSigScript,
  )
import Sophie.Spec.Ledger.TxBody (TxBody (..))

data ExampleEra c

instance CryptoClass.Crypto c => Era (ExampleEra c) where
  type Crypto (ExampleEra c) = c

instance CryptoClass.Crypto c => UsesValue (ExampleEra c)

instance CryptoClass.Crypto c => UsesTxOut (ExampleEra c) where
  makeTxOut _ a v = TxOut a v

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Tx (ExampleEra c) = Tx (ExampleEra c)

type instance Core.Value (ExampleEra _c) = Coin

type instance Core.TxBody (ExampleEra c) = TxBody (ExampleEra c)

type instance Core.TxOut (ExampleEra c) = TxOut (ExampleEra c)

type instance Core.Script (ExampleEra c) = MultiSig c

type instance Core.AuxiliaryData (ExampleEra c) = Metadata (ExampleEra c)

type instance Core.PParams (ExampleEra c) = SPP.PParams (ExampleEra c)

type instance Core.PParamsDelta (ExampleEra c) = SPP.PParamsUpdate (ExampleEra c)

type instance Core.Witnesses (ExampleEra c) = WitnessSet (ExampleEra c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  (CryptoClass.Crypto c) =>
  UsesPParams (ExampleEra c)
  where
  mergePPUpdates _ = SPP.updatePParams

nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

instance
  (CryptoClass.Crypto c, UsesTxBody (ExampleEra c)) =>
  ValidateScript (ExampleEra c)
  where
  validateScript = validateNativeMultiSigScript
  scriptPrefixTag _script = nativeMultiSigTag

instance CryptoClass.Crypto c => SupportsSegWit (ExampleEra c) where
  type TxSeq (ExampleEra c) = Sophie.TxSeq (ExampleEra c)
  fromTxSeq = Sophie.txSeqTxns
  toTxSeq = Sophie.TxSeq
  hashTxSeq = Sophie.bbHash
  numSegComponents = 3

instance CryptoClass.Crypto c => ValidateAuxiliaryData (ExampleEra c) c where
  hashAuxiliaryData metadata = AuxiliaryDataHash (makeHashWithExplicitProxys (Proxy @c) index metadata)
    where
      index = Proxy @EraIndependentAuxiliaryData
  validateAuxiliaryData (Metadata m) = all validMetadatum m

instance OptimumCrypto c => ApplyTx (ExampleEra c)

instance OptimumCrypto c => ApplyBlock (ExampleEra c)

instance OptimumCrypto c => GetLedgerView (ExampleEra c)

instance OptimumCrypto c => SophieBasedEra (ExampleEra c)

instance (CryptoClass.Crypto c) => CanStartFromGenesis (ExampleEra c) where
  initialState sg () =
    NewEpochState
      initialEpochNo
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      ( EpochState
          (AccountState (Coin 0) reserves)
          emptySnapShots
          ( LedgerState
              ( UTxOState
                  initialUtxo
                  (Coin 0)
                  (Coin 0)
                  def
              )
              (DPState (def {_genDelegs = GenDelegs genDelegs}) def)
          )
          pp
          pp
          def
      )
      SNothing
      (PoolDistr Map.empty)
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        word64ToCoin (sgMaxEntropicSupply sg)
          <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

-- These rules are all inherited from Sophie
-- The types on the right are all instances of class STS, ultimately defined in Control.State.Transition.Extended
type instance Core.EraRule "BBODY" (ExampleEra c) = BBODY (ExampleEra c) -- Block body

type instance Core.EraRule "DELEG" (ExampleEra c) = DELEG (ExampleEra c)

type instance Core.EraRule "DELEGS" (ExampleEra c) = DELEGS (ExampleEra c)

type instance Core.EraRule "DELPL" (ExampleEra c) = DELPL (ExampleEra c)

type instance Core.EraRule "EPOCH" (ExampleEra c) = EPOCH (ExampleEra c)

type instance Core.EraRule "LEDGER" (ExampleEra c) = LEDGER (ExampleEra c)

type instance Core.EraRule "LEDGERS" (ExampleEra c) = LEDGERS (ExampleEra c)

type instance Core.EraRule "MIR" (ExampleEra c) = MIR (ExampleEra c)

type instance Core.EraRule "NEWEPOCH" (ExampleEra c) = NEWEPOCH (ExampleEra c)

type instance Core.EraRule "NEWPP" (ExampleEra c) = NEWPP (ExampleEra c)

type instance Core.EraRule "OCERT" (ExampleEra c) = OCERT (ExampleEra c)

type instance Core.EraRule "OVERLAY" (ExampleEra c) = OVERLAY (ExampleEra c)

type instance Core.EraRule "POOL" (ExampleEra c) = POOL (ExampleEra c)

type instance Core.EraRule "POOLREAP" (ExampleEra c) = POOLREAP (ExampleEra c)

type instance Core.EraRule "PPUP" (ExampleEra c) = PPUP (ExampleEra c)

type instance Core.EraRule "RUPD" (ExampleEra c) = RUPD (ExampleEra c)

type instance Core.EraRule "SNAP" (ExampleEra c) = SNAP (ExampleEra c)

type instance Core.EraRule "TICK" (ExampleEra c) = TICK (ExampleEra c)

type instance Core.EraRule "TICKF" (ExampleEra c) = TICKF (ExampleEra c)

type instance Core.EraRule "TICKN" (ExampleEra _c) = TICKN

type instance Core.EraRule "UPEC" (ExampleEra c) = UPEC (ExampleEra c)

type instance Core.EraRule "UTXO" (ExampleEra c) = UTXO (ExampleEra c)

type instance Core.EraRule "UTXOW" (ExampleEra c) = UTXOW (ExampleEra c)
