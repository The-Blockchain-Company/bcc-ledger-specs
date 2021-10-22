{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Ledger.SophieMA.Rules.EraMapping () where

import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.SophieMA (SophieMAEra)
import Bcc.Ledger.SophieMA.Rules.Utxo (UTXO)
import Bcc.Ledger.SophieMA.Rules.Utxow (UTXOW)
import qualified Bcc.Protocol.TOptimum.Rules.OCert as Sophie
import qualified Bcc.Protocol.TOptimum.Rules.Overlay as Sophie
import qualified Sophie.Spec.Ledger.API as Sophie
import qualified Sophie.Spec.Ledger.STS.Bbody as Sophie
import qualified Sophie.Spec.Ledger.STS.Epoch as Sophie
import qualified Sophie.Spec.Ledger.STS.Mir as Sophie
import qualified Sophie.Spec.Ledger.STS.Newpp as Sophie
import qualified Sophie.Spec.Ledger.STS.Rupd as Sophie
import qualified Sophie.Spec.Ledger.STS.Snap as Sophie
import qualified Sophie.Spec.Ledger.STS.Tick as Sophie
import qualified Sophie.Spec.Ledger.STS.Upec as Sophie

-- These rules are all inherited from Sophie

type instance Core.EraRule "BBODY" (SophieMAEra ma c) = Sophie.BBODY (SophieMAEra ma c)

type instance Core.EraRule "DELEG" (SophieMAEra ma c) = Sophie.DELEG (SophieMAEra ma c)

type instance Core.EraRule "DELEGS" (SophieMAEra ma c) = Sophie.DELEGS (SophieMAEra ma c)

type instance Core.EraRule "DELPL" (SophieMAEra ma c) = Sophie.DELPL (SophieMAEra ma c)

type instance Core.EraRule "EPOCH" (SophieMAEra ma c) = Sophie.EPOCH (SophieMAEra ma c)

type instance Core.EraRule "LEDGER" (SophieMAEra ma c) = Sophie.LEDGER (SophieMAEra ma c)

type instance Core.EraRule "LEDGERS" (SophieMAEra ma c) = Sophie.LEDGERS (SophieMAEra ma c)

type instance Core.EraRule "MIR" (SophieMAEra ma c) = Sophie.MIR (SophieMAEra ma c)

type instance Core.EraRule "NEWEPOCH" (SophieMAEra ma c) = Sophie.NEWEPOCH (SophieMAEra ma c)

type instance Core.EraRule "NEWPP" (SophieMAEra ma c) = Sophie.NEWPP (SophieMAEra ma c)

type instance Core.EraRule "OCERT" (SophieMAEra ma c) = Sophie.OCERT (SophieMAEra ma c)

type instance Core.EraRule "OVERLAY" (SophieMAEra ma c) = Sophie.OVERLAY (SophieMAEra ma c)

type instance Core.EraRule "POOL" (SophieMAEra ma c) = Sophie.POOL (SophieMAEra ma c)

type instance Core.EraRule "POOLREAP" (SophieMAEra ma c) = Sophie.POOLREAP (SophieMAEra ma c)

type instance Core.EraRule "PPUP" (SophieMAEra ma c) = Sophie.PPUP (SophieMAEra ma c)

type instance Core.EraRule "RUPD" (SophieMAEra ma c) = Sophie.RUPD (SophieMAEra ma c)

type instance Core.EraRule "SNAP" (SophieMAEra ma c) = Sophie.SNAP (SophieMAEra ma c)

type instance Core.EraRule "TICK" (SophieMAEra ma c) = Sophie.TICK (SophieMAEra ma c)

type instance Core.EraRule "TICKF" (SophieMAEra ma c) = Sophie.TICKF (SophieMAEra ma c)

type instance Core.EraRule "TICKN" (SophieMAEra _ma _c) = Sophie.TICKN

type instance Core.EraRule "UPEC" (SophieMAEra ma c) = Sophie.UPEC (SophieMAEra ma c)

-- These rules are defined anew in the SophieMA era(s)

type instance Core.EraRule "UTXO" (SophieMAEra ma c) = UTXO (SophieMAEra ma c)

type instance Core.EraRule "UTXOW" (SophieMAEra ma c) = UTXOW (SophieMAEra ma c)
