{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Chain.Common.EntropicPortion
  ( EntropicPortion,
    rationalToEntropicPortion,
    entropicPortionToRational,
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..))
import Bcc.Prelude
import Control.Monad (fail)
import qualified Data.Aeson as Aeson
import Formatting (bprint, build, float, int, sformat)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Quiet
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | 'EntropicPortion' is a legacy Cole type that we keep only for
-- compatibility. It was originally intended to represent a fraction of stake
-- in the system. It is used only for the thresholds used in the update system
-- rules, most of which are now themselves unused. The remaining case is no
-- longer interpreted as a fraction of all stake, but as a fraction of the
-- number of genesis keys.
--
-- It has enormous precision, due to the fact that it was originally intended
-- to represent a fraction of all stake and can cover the precision of all the
-- Entropic in the system.
--
-- It is represented as a rational nominator with a fixed implicit denominator
-- of 1e15. So the nominator must be in the range @[0..1e15]@. This is also the
-- representation used on-chain (in update proposals) and in the JSON
-- genesis file.
--
-- It is interpreted as a 'Rational' via the provided conversion functions.
newtype EntropicPortion = EntropicPortion
  { unEntropicPortion :: Word64
  }
  deriving (Ord, Eq, Generic, HeapWords, NFData, NoThunks)
  deriving (Show) via (Quiet EntropicPortion)

instance B.Buildable EntropicPortion where
  build cp@(EntropicPortion x) =
    bprint
      (int . "/" . int . " (approx. " . float . ")")
      x
      entropicPortionDenominator
      (fromRational (entropicPortionToRational cp) :: Double)

-- Used for debugging purposes only
instance Aeson.ToJSON EntropicPortion

instance ToCBOR EntropicPortion where
  toCBOR = toCBOR . unEntropicPortion

instance FromCBOR EntropicPortion where
  fromCBOR = do
    nominator <- fromCBOR
    when (nominator > entropicPortionDenominator) $
      fail "EntropicPortion: value out of bounds [0..1e15]"
    return (EntropicPortion nominator)

-- The canonical JSON instance for EntropicPortion uses only the nominator in
-- the external representation,  rather than a real in the range [0,1].
-- This is because 'canonical-json' only supports numbers of type @Int54@.
instance Monad m => ToJSON m EntropicPortion where
  toJSON = toJSON . unEntropicPortion

instance MonadError SchemaError m => FromJSON m EntropicPortion where
  fromJSON val = do
    nominator <- fromJSON val
    when (nominator > entropicPortionDenominator) $
      throwError
        SchemaError
          { seExpected = "EntropicPortion integer in bounds [0..1e15]",
            seActual = Just (sformat build nominator)
          }
    pure (EntropicPortion nominator)

-- | Denominator used by 'EntropicPortion'.
entropicPortionDenominator :: Word64
entropicPortionDenominator = 1e15

-- | Make a 'EntropicPortion' from a 'Rational'
-- which must be in the range @[0..1]@.
rationalToEntropicPortion :: Rational -> EntropicPortion
rationalToEntropicPortion r
  | r >= 0 && r <= 1 =
    EntropicPortion
      (ceiling (r * toRational entropicPortionDenominator))
  | otherwise = panic "rationalToEntropicPortion: out of range [0..1]"

-- | Turn a 'EntropicPortion' into a 'Rational' in the range @[0..1]@.
entropicPortionToRational :: EntropicPortion -> Rational
entropicPortionToRational (EntropicPortion n) =
  toInteger n % toInteger entropicPortionDenominator
