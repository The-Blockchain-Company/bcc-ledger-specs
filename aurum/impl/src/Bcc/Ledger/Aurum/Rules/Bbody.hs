{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module Bcc.Ledger.Aurum.Rules.Bbody
  ( AurumBBODY,
    AurumBbodyPredFail (..),
    AurumBbodyEvent (..),
    bbodyTransition,
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..))
import Bcc.Ledger.Aurum.Scripts (ExUnits (..), pointWiseExUnits)
import qualified Bcc.Ledger.Aurum.Tx as Aurum (ValidatedTx, totExUnits)
import Bcc.Ledger.Aurum.TxSeq (txSeqTxns)
import qualified Bcc.Ledger.Aurum.TxSeq as Aurum (TxSeq)
import Bcc.Ledger.Aurum.TxWitness (TxWitness)
import Bcc.Ledger.BaseTypes (SophieBase, UnitInterval, epochInfo)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Era (Era (Crypto), SupportsSegWit (..))
import qualified Bcc.Ledger.Era as Era
import Bcc.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Bcc.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
import Bcc.Protocol.TOptimum.BHeader
  ( BHBody (bhash, bheaderSlotNo),
    BHeader (..),
    hBbsize,
    issuerIDfromBHBody,
  )
import Bcc.Protocol.TOptimum.Rules.Overlay (isOverlaySlot)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
    (?!),
  )
import Data.Coders
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Sophie.Spec.Ledger.BlockChain
  ( Block (..),
    bBodySize,
    incrBlocks,
  )
import Sophie.Spec.Ledger.LedgerState (LedgerState)
import Sophie.Spec.Ledger.STS.Bbody
  ( BbodyEnv (..),
    BbodyEvent (..),
    BbodyPredicateFailure (..),
    BbodyState (..),
  )
import Sophie.Spec.Ledger.STS.Ledgers (LedgersEnv (..))
import Sophie.Spec.Ledger.TxBody (EraIndependentTxBody)

-- =======================================
-- A new PredicateFailure type

data AurumBbodyPredFail era
  = SophieInAurumPredFail (BbodyPredicateFailure era)
  | TooManyExUnits
      !ExUnits
      -- ^ Computed Sum of ExUnits for all zerepoch scripts
      !ExUnits
      -- ^ Maximum allowed by protocal parameters
  deriving (Generic)

newtype AurumBbodyEvent era
  = SophieInAurumEvent (BbodyEvent era)

deriving instance
  (Era era, Show (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Show (AurumBbodyPredFail era)

deriving instance
  (Era era, Eq (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Eq (AurumBbodyPredFail era)

deriving anyclass instance
  (Era era, NoThunks (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  NoThunks (AurumBbodyPredFail era)

instance
  ( Typeable era,
    ToCBOR (BbodyPredicateFailure era)
  ) =>
  ToCBOR (AurumBbodyPredFail era)
  where
  toCBOR (SophieInAurumPredFail x) = encode (Sum SophieInAurumPredFail 0 !> To x)
  toCBOR (TooManyExUnits x y) = encode (Sum TooManyExUnits 1 !> To x !> To y)

instance
  ( Typeable era,
    FromCBOR (BbodyPredicateFailure era) -- TODO why is there no FromCBOR for (BbodyPredicateFailure era)
  ) =>
  FromCBOR (AurumBbodyPredFail era)
  where
  fromCBOR = decode (Summands "AurumBbodyPredFail" dec)
    where
      dec 0 = SumD SophieInAurumPredFail <! From
      dec 1 = SumD TooManyExUnits <! From <! From
      dec n = Invalid n

-- ========================================
-- The STS instance

-- | The uninhabited type that marks the STS Aurum Era instance.
data AurumBBODY era

bbodyTransition ::
  forall (someBBODY :: Type -> Type) era.
  ( -- Conditions that the Abstract someBBODY must meet
    STS (someBBODY era),
    Signal (someBBODY era) ~ Block era,
    PredicateFailure (someBBODY era) ~ AurumBbodyPredFail era,
    BaseM (someBBODY era) ~ SophieBase,
    State (someBBODY era) ~ BbodyState era,
    Environment (someBBODY era) ~ BbodyEnv era,
    -- Conditions to be an instance of STS
    Embed (Core.EraRule "LEDGERS" era) (someBBODY era),
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Core.Tx era),
    -- Conditions to define the rule in this Era
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_maxBlockExUnits" (Core.PParams era) ExUnits,
    Era era, -- supplies WellFormed HasField, and Crypto constraints
    Era.TxSeq era ~ Aurum.TxSeq era,
    Core.Tx era ~ Aurum.ValidatedTx era,
    Core.Witnesses era ~ TxWitness era
  ) =>
  TransitionRule (someBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account,
               BbodyState ls b,
               Block (BHeader bhb _) txsSeq
               )
           ) -> do
        let txs = txSeqTxns txsSeq
            actualBodySize = bBodySize txsSeq
            actualBodyHash = hashTxSeq @era txsSeq

        actualBodySize == fromIntegral (hBbsize bhb)
          ?! SophieInAurumPredFail
            ( WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ hBbsize bhb)
            )

        actualBodyHash == bhash bhb
          ?! SophieInAurumPredFail
            ( InvalidBodyHashBBODY @era actualBodyHash (bhash bhb)
            )

        ls' <-
          trans @(Core.EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bheaderSlotNo bhb) pp account, ls, StrictSeq.fromStrict txs)

        -- Note that this may not actually be a stake pool - it could be a
        -- genesis key delegate. However, this would only entail an overhead of
        -- 7 counts, and it's easier than differentiating here.
        --
        -- TODO move this computation inside 'incrBlocks' where it belongs. Here
        -- we make an assumption that 'incrBlocks' must enforce, better for it
        -- to be done in 'incrBlocks' where we can see that the assumption is
        -- enforced.
        let hkAsStakePool = coerceKeyRole . issuerIDfromBHBody $ bhb
            slot = bheaderSlotNo bhb
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfo
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e

        {- ∑(tx ∈ txs)(totExunits tx) ≤ maxBlockExUnits pp  -}
        let txTotal, ppMax :: ExUnits
            txTotal = foldMap Aurum.totExUnits txs
            ppMax = getField @"_maxBlockExUnits" pp
        pointWiseExUnits (<=) txTotal ppMax ?! TooManyExUnits txTotal ppMax

        pure $
          BbodyState @era
            ls'
            ( incrBlocks
                (isOverlaySlot firstSlotNo (getField @"_d" pp) slot)
                hkAsStakePool
                b
            )

instance
  ( DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Embed (Core.EraRule "LEDGERS" era) (AurumBBODY era),
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Aurum.ValidatedTx era),
    Era era,
    Core.Tx era ~ Aurum.ValidatedTx era,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_maxBlockExUnits" (Core.PParams era) ExUnits,
    Era.TxSeq era ~ Aurum.TxSeq era,
    Core.Tx era ~ Aurum.ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    SupportsSegWit era
  ) =>
  STS (AurumBBODY era)
  where
  type
    State (AurumBBODY era) =
      BbodyState era

  type
    Signal (AurumBBODY era) =
      Block era

  type Environment (AurumBBODY era) = BbodyEnv era

  type BaseM (AurumBBODY era) = SophieBase

  type PredicateFailure (AurumBBODY era) = AurumBbodyPredFail era
  type Event (AurumBBODY era) = AurumBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition @AurumBBODY]

instance
  ( Era era,
    BaseM ledgers ~ SophieBase,
    ledgers ~ Core.EraRule "LEDGERS" era,
    STS ledgers,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era
  ) =>
  Embed ledgers (AurumBBODY era)
  where
  wrapFailed = SophieInAurumPredFail . LedgersFailure
  wrapEvent = SophieInAurumEvent . LedgersEvent
