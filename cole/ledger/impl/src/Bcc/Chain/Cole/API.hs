-- | Auxiliary definitions to make working with the Cole ledger easier
module Bcc.Chain.Cole.API
  ( -- * Extract info from chain state
    getDelegationMap,
    getMaxBlockSize,

    -- * Applying blocks
    module Bcc.Chain.Cole.API.Validation,

    -- * Applying transactions
    module Bcc.Chain.Cole.API.Mempool,

    -- * Protocol
    module Bcc.Chain.Cole.API.Protocol,

    -- * Annotations
    reAnnotateBlock,
    reAnnotateBoundary,
    reAnnotateUsing,

    -- * Headers
    abobMatchesBody,
  )
where

import Bcc.Chain.Cole.API.Common
import Bcc.Chain.Cole.API.Mempool
import Bcc.Chain.Cole.API.Protocol
import Bcc.Chain.Cole.API.Validation
