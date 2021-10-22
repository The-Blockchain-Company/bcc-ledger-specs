{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bcc.Chain.Slotting.SlotCount
  ( SlotCount (..),
  )
where

import Bcc.Binary (FromCBOR, ToCBOR)
import Bcc.Prelude
import Formatting.Buildable (Buildable)

-- | A number of slots
newtype SlotCount = SlotCount
  { unSlotCount :: Word64
  }
  deriving stock (Read, Show, Generic)
  deriving newtype (Eq, Ord, Buildable, ToCBOR, FromCBOR)
  deriving anyclass (NFData)
