{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Ledger.SophieMA.Rules.Utxow where

import Bcc.Ledger.Address (Addr)
import Bcc.Ledger.BaseTypes
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Era (Era (Crypto))
import Bcc.Ledger.SophieMA.Rules.Utxo (UTXO, UtxoPredicateFailure)
import Bcc.Ledger.SophieMA.TxBody ()
import Control.State.Transition.Extended
import GHC.Records (HasField)
import Sophie.Spec.Ledger.LedgerState
  ( UTxOState,
    witsVKeyNeeded,
  )
import qualified Sophie.Spec.Ledger.STS.Ledger as Sophie
import Sophie.Spec.Ledger.STS.Utxo (UtxoEnv)
import Sophie.Spec.Ledger.STS.Utxow
  ( SophieStyleWitnessNeeds,
    UtxowEvent (..),
    UtxowPredicateFailure (..),
    sophieStyleWitness,
  )
import Sophie.Spec.Ledger.Tx (WitnessSet)

-- ==============================================================================
--   We want to reuse the same rules for Jen and Evie. We accomplish this
--   by adding: HasField "minted" (Core.TxBody era) (Set (ScriptHash (Crypto era)))
--   to the (WellFormed era) constraint, and adjusting UTxO.(ScriptsNeeded) to
--   add this set to its output. In the Sophie and Evie Era, this is the empty set.
--   With this generalization, Sophie.Spec.Ledger.STS.Utxow(sophieStyleWitness)
--   can still be used in Evie and Jen, because they use the same Sophie style rules.

--------------------------------------------------------------------------------
-- UTXOW STS
--------------------------------------------------------------------------------

data UTXOW era

instance
  forall era.
  ( -- Fix Core.Witnesses to the Evie and Jen Era
    Core.Witnesses era ~ WitnessSet era,
    HasField "address" (Core.TxOut era) (Addr (Crypto era)),
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (UTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Core.Tx era,
    -- Supply the HasField and Validate instances for Jen and Evie (which match Sophie)
    SophieStyleWitnessNeeds era
  ) =>
  STS (UTXOW era)
  where
  type State (UTXOW era) = UTxOState era
  type Signal (UTXOW era) = Core.Tx era
  type Environment (UTXOW era) = UtxoEnv era
  type BaseM (UTXOW era) = SophieBase
  type PredicateFailure (UTXOW era) = UtxowPredicateFailure era
  type Event (UTXOW era) = UtxowEvent era

  transitionRules = [sophieStyleWitness witsVKeyNeeded id]

  -- The SophieMA Era uses the same PredicateFailure type
  -- as Sophie, so the 'embed' function is identity
  initialRules = []

instance
  ( Era era,
    STS (UTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ UtxoPredicateFailure era,
    Event (Core.EraRule "UTXO" era) ~ Event (UTXO era)
  ) =>
  Embed (UTXO era) (UTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = UtxoEvent

instance
  ( Era era,
    STS (UTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ UtxowPredicateFailure era,
    Event (Core.EraRule "UTXOW" era) ~ Event (UTXOW era)
  ) =>
  Embed (UTXOW era) (Sophie.LEDGER era)
  where
  wrapFailed = Sophie.UtxowFailure
  wrapEvent = Sophie.UtxowEvent
