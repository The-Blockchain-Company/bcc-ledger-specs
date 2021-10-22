{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sophie.Spec.Ledger.STS.Ledgers
  ( LEDGERS,
    LedgersEnv (..),
    LedgersPredicateFailure (..),
    LedgersEvent (..),
    PredicateFailure,
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..))
import Bcc.Ledger.BaseTypes (SophieBase)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Era
import Bcc.Ledger.Keys (DSignable, Hash)
import Bcc.Ledger.Sophie.Constraints (SophieBased)
import Bcc.Ledger.Slot (SlotNo)
import Control.Monad (foldM)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Default.Class (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Sophie.Spec.Ledger.LedgerState
  ( AccountState,
    DPState,
    LedgerState (..),
    UTxOState,
    _delegationState,
    _utxoState,
  )
import Sophie.Spec.Ledger.STS.Ledger
  ( LEDGER,
    LedgerEnv (..),
    LedgerEvent,
    LedgerPredicateFailure,
  )
import Sophie.Spec.Ledger.TxBody (EraIndependentTxBody)

data LEDGERS era

data LedgersEnv era = LedgersEnv
  { ledgersSlotNo :: SlotNo,
    ledgersPp :: Core.PParams era,
    ledgersAccount :: AccountState
  }

newtype LedgersPredicateFailure era
  = LedgerFailure (PredicateFailure (Core.EraRule "LEDGER" era)) -- Subtransition Failures
  deriving (Generic)

newtype LedgersEvent era
  = LedgerEvent (Event (Core.EraRule "LEDGER" era))

deriving stock instance
  ( SophieBased era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Show (LedgersPredicateFailure era)

deriving stock instance
  ( SophieBased era,
    Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Eq (LedgersPredicateFailure era)

instance
  ( SophieBased era,
    NoThunks (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  NoThunks (LedgersPredicateFailure era)

instance
  ( SophieBased era,
    ToCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  ToCBOR (LedgersPredicateFailure era)
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  ( SophieBased era,
    FromCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  FromCBOR (LedgersPredicateFailure era)
  where
  fromCBOR = LedgerFailure <$> fromCBOR

instance
  ( Era era,
    SophieBased era,
    Embed (Core.EraRule "LEDGER" era) (LEDGERS era),
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Default (LedgerState era)
  ) =>
  STS (LEDGERS era)
  where
  type State (LEDGERS era) = LedgerState era
  type Signal (LEDGERS era) = Seq (Core.Tx era)
  type Environment (LEDGERS era) = LedgersEnv era
  type BaseM (LEDGERS era) = SophieBase
  type PredicateFailure (LEDGERS era) = LedgersPredicateFailure era
  type Event (LEDGERS era) = LedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (Core.EraRule "LEDGER" era) (LEDGERS era),
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let (u, dp) = (_utxoState ls, _delegationState ls)
  (u'', dp'') <-
    foldM
      ( \(u', dp') (ix, tx) ->
          trans @(Core.EraRule "LEDGER" era) $
            TRC (LedgerEnv slot ix pp account, (u', dp'), tx)
      )
      (u, dp)
      $ zip [0 ..] $
        toList txwits

  pure $ LedgerState u'' dp''

instance
  ( Era era,
    STS (LEDGER era),
    PredicateFailure (Core.EraRule "LEDGER" era) ~ LedgerPredicateFailure era,
    Event (Core.EraRule "LEDGER" era) ~ LedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent
