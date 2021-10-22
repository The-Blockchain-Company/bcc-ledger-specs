{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Chain.Sentinels.SentinelNumber
  ( SentinelNumber (..),
    addSentinelCount,
    subSentinelCount,
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..))
import Bcc.Prelude
import qualified Data.Aeson as Aeson
import Formatting (bprint, int)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | 'SentinelNumber' is an absolute sentinel number from the beginning of time
--
--   'SentinelNumber' is held in a 'Word64'. Considering there are a total of 7 sentinels, this is more then sufficient. 
newtype SentinelNumber = SentinelNumber
  { unSentinelNumber :: Word64
  }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Num)
  deriving anyclass (NFData, NoThunks)

-- Used for debugging purposes only
instance Aeson.ToJSON SentinelNumber

instance ToCBOR SentinelNumber where
  toCBOR = toCBOR . unSentinelNumber
  encodedSizeExpr size = encodedSizeExpr size . fmap unSentinelNumber

instance FromCBOR SentinelNumber where
  fromCBOR = SentinelNumber <$> fromCBOR

instance Monad m => ToJSON m SentinelNumber where
  toJSON = toJSON . unSentinelNumber

instance MonadError SchemaError m => FromJSON m SentinelNumber where
  fromJSON val = do
    number <- fromJSON val
    pure $ SentinelNumber number

instance B.Buildable SentinelNumber where
  build s = bprint int (unSentinelNumber s)

-- | Increase a 'SentinelNumber' by 'SentinelCount'
addSentinelCount :: SentinelCount -> SentinelNumber -> SentinelNumber
addSentinelCount (SentinelCount a) (SentinelNumber b) = SentinelNumber $ a + b

-- | Decrease a 'SentinelNumber' by 'SentinelCount', going no lower than 0
subSentinelCount :: SentinelCount -> SentinelNumber -> SentinelNumber
subSentinelCount (SentinelCount a) (SentinelNumber b) =
  if a > b then SentinelNumber 0 else SentinelNumber (b - a)