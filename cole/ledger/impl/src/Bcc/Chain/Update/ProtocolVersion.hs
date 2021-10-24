{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bcc.Chain.Update.ProtocolVersion
  ( ProtocolVersion (..),
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import Bcc.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint, shown)
import Formatting.Buildable (Buildable (..))
import NoThunks.Class (NoThunks (..))
import qualified Prelude

-- | Communication protocol version
data ProtocolVersion = ProtocolVersion
  { pvMajor :: !Word16,
    pvSentry  :: !Word16
  }
  deriving (Eq, Generic, Ord)
  deriving anyclass (NFData, NoThunks)

instance Show ProtocolVersion where
  show pv =
    intercalate "." [show (pvMajor pv), show (pvSentry pv)]

instance Buildable ProtocolVersion where
  build = bprint shown

-- Used for debugging purposes only
instance ToJSON ProtocolVersion

instance ToCBOR ProtocolVersion where
  toCBOR pv =
    encodeListLen 2 <> toCBOR (pvMajor pv) <> toCBOR (pvSentry pv)

  encodedSizeExpr f pv =
    1
      + encodedSizeExpr f (pvMajor <$> pv)
      + encodedSizeExpr f (pvSentry <$> pv)
     
instance FromCBOR ProtocolVersion where
  fromCBOR = do
    enforceSize "ProtocolVersion" 2
    ProtocolVersion <$> fromCBOR <*> fromCBOR
