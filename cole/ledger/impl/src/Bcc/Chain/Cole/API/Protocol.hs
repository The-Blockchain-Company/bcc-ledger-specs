module Bcc.Chain.Cole.API.Protocol
  ( previewDelegationMap,
  )
where

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Chain.Delegation as Delegation
import qualified Bcc.Chain.Delegation.Validation.Interface as D.Iface
import qualified Bcc.Chain.Slotting as CC
import Bcc.Prelude

-- | Preview the delegation map at a slot assuming no new delegations are
-- | scheduled.
previewDelegationMap ::
  CC.SlotNumber ->
  CC.ChainValidationState ->
  Delegation.Map
previewDelegationMap slot cvs =
  let ds = D.Iface.activateDelegations slot $ CC.cvsDelegationState cvs
   in D.Iface.delegationMap ds
