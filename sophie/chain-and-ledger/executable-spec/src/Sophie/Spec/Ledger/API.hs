{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | API to the Sophie ledger
module Sophie.Spec.Ledger.API
  ( module X,
    SophieBasedEra,
  )
where

import Bcc.Ledger.Core (ChainData, SerialisableData)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Era (Crypto)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Sophie.Constraints
  ( UsesAuxiliary,
    UsesPParams,
    UsesScript,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
import Control.State.Transition (State)
import Sophie.Spec.Ledger.API.ColeTranslation as X
import Sophie.Spec.Ledger.API.Genesis as X
import Sophie.Spec.Ledger.API.Mempool as X
import Sophie.Spec.Ledger.API.Protocol as X
import Sophie.Spec.Ledger.API.Types as X
import Sophie.Spec.Ledger.API.Validation as X
import Sophie.Spec.Ledger.API.Wallet as X

class
  ( OptimumCrypto (Crypto era),
    GetLedgerView era,
    ApplyBlock era,
    ApplyTx era,
    CanStartFromGenesis era,
    UsesValue era,
    UsesScript era,
    UsesAuxiliary era,
    UsesTxBody era,
    UsesTxOut era,
    UsesPParams era,
    ChainData (State (Core.EraRule "PPUP" era)),
    SerialisableData (State (Core.EraRule "PPUP" era))
  ) =>
  SophieBasedEra era

instance OptimumCrypto crypto => SophieBasedEra (SophieEra crypto)
