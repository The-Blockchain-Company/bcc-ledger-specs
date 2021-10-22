module Sophie.Spec.Ledger.Delegation.PoolParams
  ( poolSpec,
  )
where

import Bcc.Ledger.BaseTypes (UnitInterval)
import Bcc.Ledger.Coin (Coin)
import Sophie.Spec.Ledger.TxBody (PoolParams (..))

poolSpec :: PoolParams crypto -> (Coin, UnitInterval, Coin)
poolSpec pool = (_poolCost pool, _poolMargin pool, _poolPledge pool)
