{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Ledger.Aurum
  ( AurumEra,
    Self,
    TxOut,
    Value,
    TxBody,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
  )
where

import Bcc.Ledger.Aurum.Data (AuxiliaryData (..))
import Bcc.Ledger.Aurum.Genesis
import Bcc.Ledger.Aurum.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    updatePParams,
  )
import qualified Bcc.Ledger.Aurum.Rules.Bbody as Aurum (AurumBBODY)
import qualified Bcc.Ledger.Aurum.Rules.Ledger as Aurum (AurumLEDGER)
import Bcc.Ledger.Aurum.Rules.Utxo (utxoEntrySize)
import qualified Bcc.Ledger.Aurum.Rules.Utxo as Aurum (AurumUTXO)
import qualified Bcc.Ledger.Aurum.Rules.Utxos as Aurum (UTXOS)
import qualified Bcc.Ledger.Aurum.Rules.Utxow as Aurum (AurumUTXOW)
import Bcc.Ledger.Aurum.Scripts (Script (..), isZerepochScript)
import Bcc.Ledger.Aurum.Tx (ValidatedTx (..), minfee)
import Bcc.Ledger.Aurum.TxBody (TxBody, TxOut (..))
import Bcc.Ledger.Aurum.TxInfo (validScript)
import qualified Bcc.Ledger.Aurum.TxSeq as Aurum (TxSeq (..), hashTxSeq)
import Bcc.Ledger.Aurum.TxWitness (TxWitness (..))
import Bcc.Ledger.AuxiliaryData (AuxiliaryDataHash (..), ValidateAuxiliaryData (..))
import qualified Bcc.Ledger.BaseTypes as Sophie
import Bcc.Ledger.Coin
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CC
import qualified Bcc.Ledger.Era as EraModule
import Bcc.Ledger.Keys (GenDelegs (GenDelegs), VestedDelegs(VestedDelegs))
import qualified Bcc.Ledger.Jen.Value as V (Value)
import Bcc.Ledger.Rules.ValidationMode
  ( applySTSNonStatic,
  )
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Ledger.Sophie (nativeMultiSigTag)
import Bcc.Ledger.Sophie.Constraints
  ( UsesPParams (..),
    UsesTxOut (..),
    UsesValue,
  )
import Bcc.Ledger.SophieMA.Rules.Utxo (consumed)
import Bcc.Ledger.SophieMA.Timelocks (validateTimelock)
import Bcc.Ledger.Val (Val (inject), coin, (<->))
import Bcc.Protocol.TOptimum (PoolDistr (..))
import qualified Bcc.Protocol.TOptimum.Rules.OCert as Sophie
import qualified Bcc.Protocol.TOptimum.Rules.Overlay as Sophie
import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition.Extended (TRC (TRC))
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Set as Set
import qualified Sophie.Spec.Ledger.API as API
import Sophie.Spec.Ledger.EpochBoundary
import Sophie.Spec.Ledger.Genesis (genesisUTxO, sgGenDelegs, sgMaxEntropicSupply, sgProtocolParams, sgVestedDelegs)
import Sophie.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
    _genDelegs,
    _vestedDelegs,
  )
import Sophie.Spec.Ledger.Metadata (validMetadatum)
import qualified Sophie.Spec.Ledger.STS.Epoch as Sophie
import qualified Sophie.Spec.Ledger.STS.Mir as Sophie
import qualified Sophie.Spec.Ledger.STS.Newpp as Sophie
import qualified Sophie.Spec.Ledger.STS.Rupd as Sophie
import qualified Sophie.Spec.Ledger.STS.Snap as Sophie
import qualified Sophie.Spec.Ledger.STS.Tick as Sophie
import qualified Sophie.Spec.Ledger.STS.Upec as Sophie
import qualified Sophie.Spec.Ledger.Tx as Sophie
import Sophie.Spec.Ledger.UTxO (balance)

-- =====================================================

-- | The Aurum era
data AurumEra c

instance
  ( CC.Crypto c,
    era ~ AurumEra c
  ) =>
  EraModule.Era (AurumEra c)
  where
  type Crypto (AurumEra c) = c

instance API.OptimumCrypto c => API.ApplyTx (AurumEra c) where
  reapplyTx globals env state vtx =
    let res =
          flip runReader globals
            . applySTSNonStatic
              @(Core.EraRule "LEDGER" (AurumEra c))
            $ TRC (env, state, API.extractTx vtx)
     in liftEither . left API.ApplyTxError $ res

instance API.OptimumCrypto c => API.ApplyBlock (AurumEra c)

instance (API.OptimumCrypto c) => API.GetLedgerView (AurumEra c)

instance (CC.Crypto c) => Sophie.ValidateScript (AurumEra c) where
  isNativeScript x = not (isZerepochScript x)
  scriptPrefixTag script =
    if isZerepochScript script
      then "\x01"
      else nativeMultiSigTag -- "\x00"
  validateScript (TimelockScript script) tx = validateTimelock @(AurumEra c) script tx
  validateScript (ZerepochScript _) _tx = True

-- To run a ZerepochScript use Bcc.Ledger.Aurum.TxInfo(runPLCScript)
-- To run any Aurum Script use Bcc.Ledger.Aurum.ZerepochScriptApi(evalScripts)
-- hashScript x = ...  We use the default method for hashScript

instance
  ( CC.Crypto c
  ) =>
  API.CanStartFromGenesis (AurumEra c)
  where
  type AdditionalGenesisConfig (AurumEra c) = AurumGenesis

  initialState sg ag =
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
              (DPState (def {_genDelegs = GenDelegs genDelegs} 
                            {_vestedDelegs = VestedDelegs vestedDelegs}) def)
          )
          (extendPPWithGenesis pp ag)
          (extendPPWithGenesis pp ag)
          def
      )
      SNothing
      (PoolDistr Map.empty)
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        coin $
          inject (word64ToCoin (sgMaxEntropicSupply sg))
            <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      vestedDelegs = sgVestedDelegs sg
      pp = sgProtocolParams sg

instance (CC.Crypto c) => UsesTxOut (AurumEra c) where
  makeTxOut _proxy addr val = TxOut addr val Sophie.SNothing

instance CC.Crypto c => API.CLI (AurumEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses (ValidatedTx b ws aux iv) newWits = ValidatedTx b ws' aux iv
    where
      ws' = ws {txwitsVKey = Set.union newWits (txwitsVKey ws)}

  evaluateMinEntropicOutput pp out =
    Coin $ utxoEntrySize out * unCoin (_coinsPerUTxOWord pp)

type instance Core.Tx (AurumEra c) = ValidatedTx (AurumEra c)

type instance Core.TxOut (AurumEra c) = TxOut (AurumEra c)

type instance Core.TxBody (AurumEra c) = TxBody (AurumEra c)

type instance Core.Value (AurumEra c) = V.Value c

type instance Core.Script (AurumEra c) = Script (AurumEra c)

type instance Core.AuxiliaryData (AurumEra c) = AuxiliaryData (AurumEra c)

type instance Core.PParams (AurumEra c) = PParams (AurumEra c)

type instance Core.Witnesses (AurumEra c) = TxWitness (AurumEra c)

type instance Core.PParamsDelta (AurumEra c) = PParamsUpdate (AurumEra c)

instance CC.Crypto c => UsesValue (AurumEra c)

instance (CC.Crypto c) => UsesPParams (AurumEra c) where
  mergePPUpdates _ = updatePParams

instance CC.Crypto c => ValidateAuxiliaryData (AurumEra c) c where
  hashAuxiliaryData x = AuxiliaryDataHash (hashAnnotated x)
  validateAuxiliaryData (AuxiliaryData metadata scrips) =
    all validMetadatum metadata
      && all validScript scrips

instance CC.Crypto c => EraModule.SupportsSegWit (AurumEra c) where
  type TxSeq (AurumEra c) = Aurum.TxSeq (AurumEra c)
  fromTxSeq = Aurum.txSeqTxns
  toTxSeq = Aurum.TxSeq
  hashTxSeq = Aurum.hashTxSeq
  numSegComponents = 4

instance API.OptimumCrypto c => API.SophieBasedEra (AurumEra c)

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Aurum

type instance Core.EraRule "UTXOS" (AurumEra c) = Aurum.UTXOS (AurumEra c)

type instance Core.EraRule "UTXO" (AurumEra c) = Aurum.AurumUTXO (AurumEra c)

type instance Core.EraRule "UTXOW" (AurumEra c) = Aurum.AurumUTXOW (AurumEra c)

type instance Core.EraRule "LEDGER" (AurumEra c) = Aurum.AurumLEDGER (AurumEra c)

type instance Core.EraRule "BBODY" (AurumEra c) = Aurum.AurumBBODY (AurumEra c)

-- Rules inherited from Sophie

type instance Core.EraRule "DELEG" (AurumEra c) = API.DELEG (AurumEra c)

type instance Core.EraRule "DELEGS" (AurumEra c) = API.DELEGS (AurumEra c)

type instance Core.EraRule "DELPL" (AurumEra c) = API.DELPL (AurumEra c)

type instance Core.EraRule "EPOCH" (AurumEra c) = Sophie.EPOCH (AurumEra c)

type instance Core.EraRule "LEDGERS" (AurumEra c) = API.LEDGERS (AurumEra c)

type instance Core.EraRule "MIR" (AurumEra c) = Sophie.MIR (AurumEra c)

type instance Core.EraRule "NEWEPOCH" (AurumEra c) = API.NEWEPOCH (AurumEra c)

type instance Core.EraRule "NEWPP" (AurumEra c) = Sophie.NEWPP (AurumEra c)

type instance Core.EraRule "OCERT" (AurumEra c) = Sophie.OCERT (AurumEra c)

type instance Core.EraRule "OVERLAY" (AurumEra c) = Sophie.OVERLAY (AurumEra c)

type instance Core.EraRule "POOL" (AurumEra c) = API.POOL (AurumEra c)

type instance Core.EraRule "POOLREAP" (AurumEra c) = API.POOLREAP (AurumEra c)

type instance Core.EraRule "PPUP" (AurumEra c) = API.PPUP (AurumEra c)

type instance Core.EraRule "RUPD" (AurumEra c) = Sophie.RUPD (AurumEra c)

type instance Core.EraRule "SNAP" (AurumEra c) = Sophie.SNAP (AurumEra c)

type instance Core.EraRule "TICK" (AurumEra c) = Sophie.TICK (AurumEra c)

type instance Core.EraRule "TICKF" (AurumEra c) = Sophie.TICKF (AurumEra c)

type instance Core.EraRule "TICKN" (AurumEra _c) = API.TICKN

type instance Core.EraRule "UPEC" (AurumEra c) = Sophie.UPEC (AurumEra c)

-- Self-Describing type synomyms

type Self c = AurumEra c

type Value era = V.Value (EraModule.Crypto era)

type PParamsDelta era = PParamsUpdate era
