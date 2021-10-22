{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Ledger.Aurum.Rules.Ledger
  ( AurumLEDGER,
    ledgerTransition,
  )
where

import Bcc.Ledger.Aurum.Rules.Utxow (AurumEvent, AurumPredFail, AurumUTXOW)
import Bcc.Ledger.Aurum.Tx (IsValid (..), ValidatedTx (..))
import Bcc.Ledger.BaseTypes (SophieBase)
import Bcc.Ledger.Coin (Coin)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Era (Crypto, Era)
import Bcc.Ledger.Keys (DSignable, Hash)
import Control.State.Transition
  ( Assertion (..),
    AssertionViolation (..),
    Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Records (HasField, getField)
import Sophie.Spec.Ledger.EpochBoundary (obligation)
import Sophie.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    PState (..),
    UTxOState (..),
  )
import Sophie.Spec.Ledger.STS.Delegs (DELEGS, DelegsEnv (..), DelegsEvent, DelegsPredicateFailure)
import Sophie.Spec.Ledger.STS.Ledger (LedgerEnv (..), LedgerEvent (..), LedgerPredicateFailure (..))
import qualified Sophie.Spec.Ledger.STS.Ledgers as Sophie
import Sophie.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Sophie.Spec.Ledger.TxBody (DCert, EraIndependentTxBody)

-- =======================================

-- | The uninhabited type that marks the (STS Ledger) instance in the Aurum Era.
data AurumLEDGER era

-- | An abstract Aurum Era, Ledger transition. Fix 'someLedger' at a concrete type to
--   make it concrete. Depends only on the "certs" and "isValid" HasField instances.
ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( Signal (someLEDGER era) ~ Core.Tx era,
    State (someLEDGER era) ~ (UTxOState era, DPState (Crypto era)),
    Environment (someLEDGER era) ~ LedgerEnv era,
    Embed (Core.EraRule "UTXOW" era) (someLEDGER era),
    Embed (Core.EraRule "DELEGS" era) (someLEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Core.Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "isValid" (Core.Tx era) IsValid,
    Era era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, (utxoSt, dpstate), tx) <- judgmentContext
  let txbody = getField @"body" tx

  dpstate' <-
    if getField @"isValid" tx == IsValid True
      then
        trans @(Core.EraRule "DELEGS" era) $
          TRC
            ( DelegsEnv slot txIx pp tx account,
              dpstate,
              StrictSeq.fromStrict $ getField @"certs" $ txbody
            )
      else pure dpstate

  let DPState dstate pstate = dpstate
      genDelegs = _genDelegs dstate
      stpools = _pParams pstate

  utxoSt' <-
    trans @(Core.EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv @era slot pp stpools genDelegs,
          utxoSt,
          tx
        )
  pure (utxoSt', dpstate')

instance
  ( Show (Core.Script era), -- All these Show instances arise because
    Show (Core.TxBody era), -- renderAssertionViolation, turns them into strings
    Show (Core.AuxiliaryData era),
    Show (Core.PParams era),
    Show (Core.Value era),
    Show (Core.PParamsDelta era),
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era,
    Core.Tx era ~ ValidatedTx era,
    Embed (Core.EraRule "DELEGS" era) (AurumLEDGER era),
    Embed (Core.EraRule "UTXOW" era) (AurumLEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ ValidatedTx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    Show (UTxOState era)
  ) =>
  STS (AurumLEDGER era)
  where
  type
    State (AurumLEDGER era) =
      (UTxOState era, DPState (Crypto era))
  type Signal (AurumLEDGER era) = ValidatedTx era
  type Environment (AurumLEDGER era) = LedgerEnv era
  type BaseM (AurumLEDGER era) = SophieBase
  type PredicateFailure (AurumLEDGER era) = LedgerPredicateFailure era
  type Event (AurumLEDGER era) = LedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @AurumLEDGER]

  renderAssertionViolation AssertionViolation {avSTS, avMsg, avCtx, avState} =
    "AssertionViolation (" <> avSTS <> "): " <> avMsg
      <> "\n"
      <> show avCtx
      <> "\n"
      <> show avState

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (LedgerEnv {ledgerPp}, _, _))
           (utxoSt, DPState {_dstate, _pstate}) ->
              obligation ledgerPp (_rewards _dstate) (_pParams _pstate)
                == _deposited utxoSt
        )
    ]

instance
  ( Era era,
    STS (DELEGS era),
    PredicateFailure (Core.EraRule "DELEGS" era) ~ DelegsPredicateFailure era,
    Event (Core.EraRule "DELEGS" era) ~ DelegsEvent era
  ) =>
  Embed (DELEGS era) (AurumLEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era,
    STS (AurumUTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ AurumPredFail era,
    Event (Core.EraRule "UTXOW" era) ~ AurumEvent era
  ) =>
  Embed (AurumUTXOW era) (AurumLEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Era era,
    STS (AurumLEDGER era),
    PredicateFailure (Core.EraRule "LEDGER" era) ~ LedgerPredicateFailure era,
    Event (Core.EraRule "LEDGER" era) ~ LedgerEvent era
  ) =>
  Embed (AurumLEDGER era) (Sophie.LEDGERS era)
  where
  wrapFailed = Sophie.LedgerFailure
  wrapEvent = Sophie.LedgerEvent
