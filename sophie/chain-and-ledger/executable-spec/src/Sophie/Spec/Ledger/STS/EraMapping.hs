{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the mapping from conceptual rule names to concrete
-- rules for the Sophie era.
module Sophie.Spec.Ledger.STS.EraMapping () where

import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Protocol.TOptimum.Rules.OCert (OCERT)
import Bcc.Protocol.TOptimum.Rules.Overlay (OVERLAY)
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

type instance Core.EraRule "BBODY" (SophieEra c) = BBODY (SophieEra c)

type instance Core.EraRule "DELEG" (SophieEra c) = DELEG (SophieEra c)

type instance Core.EraRule "DELEGS" (SophieEra c) = DELEGS (SophieEra c)

type instance Core.EraRule "DELPL" (SophieEra c) = DELPL (SophieEra c)

type instance Core.EraRule "EPOCH" (SophieEra c) = EPOCH (SophieEra c)

type instance Core.EraRule "LEDGER" (SophieEra c) = LEDGER (SophieEra c)

type instance Core.EraRule "LEDGERS" (SophieEra c) = LEDGERS (SophieEra c)

type instance Core.EraRule "MIR" (SophieEra c) = MIR (SophieEra c)

type instance Core.EraRule "NEWEPOCH" (SophieEra c) = NEWEPOCH (SophieEra c)

type instance Core.EraRule "NEWPP" (SophieEra c) = NEWPP (SophieEra c)

type instance Core.EraRule "OCERT" (SophieEra c) = OCERT (SophieEra c)

type instance Core.EraRule "OVERLAY" (SophieEra c) = OVERLAY (SophieEra c)

type instance Core.EraRule "POOL" (SophieEra c) = POOL (SophieEra c)

type instance Core.EraRule "POOLREAP" (SophieEra c) = POOLREAP (SophieEra c)

type instance Core.EraRule "PPUP" (SophieEra c) = PPUP (SophieEra c)

type instance Core.EraRule "RUPD" (SophieEra c) = RUPD (SophieEra c)

type instance Core.EraRule "SNAP" (SophieEra c) = SNAP (SophieEra c)

type instance Core.EraRule "TICK" (SophieEra c) = TICK (SophieEra c)

type instance Core.EraRule "TICKF" (SophieEra c) = TICKF (SophieEra c)

type instance Core.EraRule "TICKN" (SophieEra c) = TICKN

type instance Core.EraRule "UPEC" (SophieEra c) = UPEC (SophieEra c)

type instance Core.EraRule "UTXO" (SophieEra c) = UTXO (SophieEra c)

type instance Core.EraRule "UTXOW" (SophieEra c) = UTXOW (SophieEra c)
