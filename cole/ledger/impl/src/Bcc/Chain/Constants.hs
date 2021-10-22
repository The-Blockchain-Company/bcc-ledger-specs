-- | Non-configurable constants
--   For configurable constants, see Bcc.Genesis.Configuration.
module Bcc.Chain.Constants
  ( sharedSeedLength,
  )
where

import Bcc.Prelude

--------------------------------------------------------------------------------
-- Constants which are not configurable
--------------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32
