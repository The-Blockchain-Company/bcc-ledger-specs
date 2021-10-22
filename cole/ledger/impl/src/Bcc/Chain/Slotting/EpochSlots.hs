{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Bcc.Chain.Slotting.EpochSlots
  ( EpochSlots (..),
    WithEpochSlots (..),
    epochFirstSlot,
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..))
import Bcc.Chain.Slotting.EpochNumber
import Bcc.Chain.Slotting.SlotNumber
import Bcc.Prelude
import Data.Data (Data)
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))

-- | The number of slots per epoch.
newtype EpochSlots = EpochSlots
  { unEpochSlots :: Word64
  }
  deriving (Data, Eq, Ord, Read, Show, Buildable, Generic, NoThunks)

instance ToCBOR EpochSlots where
  toCBOR = toCBOR . unEpochSlots

instance FromCBOR EpochSlots where
  fromCBOR = EpochSlots <$> fromCBOR

-- | Data with an accompanying slots per epoch context.
data WithEpochSlots a = WithEpochSlots
  { epochSlots :: EpochSlots,
    unWithEpochSlots :: a
  }
  deriving (Show, Eq)

-- | Calculate the first slot in an epoch.
--
-- Note that this function will give an undetermined result if Cole is not the
-- first and only era - a more robust method should use 'EpochInfo' from
-- bcc-slotting.
epochFirstSlot :: EpochSlots -> EpochNumber -> SlotNumber
epochFirstSlot (EpochSlots n) (EpochNumber k) = SlotNumber $ n * k
