{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- This is for 'mkKnownEntropic''s @n <= 45000000000000000@ constraint, which is
-- considered redundant. TODO: investigate this.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Bcc.Chain.Common.Entropic
  ( -- * Entropic
    Entropic,
    EntropicError (..),
    maxEntropicVal,

    -- * Constructors
    mkEntropic,
    mkKnownEntropic,

    -- * Formatting
    entropicF,

    -- * Conversions
    unsafeGetEntropic,
    entropicToInteger,
    integerToEntropic,

    -- * Arithmetic operations
    sumEntropic,
    addEntropic,
    subEntropic,
    scaleEntropic,
    scaleEntropicRational,
    scaleEntropicRationalUp,
    divEntropic,
    modEntropic,
  )
where

import Bcc.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    matchSize,
  )
import Bcc.Prelude
import Data.Aeson (ToJSON)
import Data.Data (Data)
import Formatting (Format, bprint, build, int, sformat)
import qualified Formatting.Buildable as B
import GHC.TypeLits (type (<=))
import NoThunks.Class (NoThunks (..))
import Quiet
import qualified Text.JSON.Canonical as Canonical
  ( FromJSON (..),
    ReportSchemaErrors,
    ToJSON (..),
  )

-- | Entropic is the least possible unit of currency
newtype Entropic = Entropic
  { unEntropic :: Word64
  }
  deriving (Ord, Eq, Generic, Data, NFData, NoThunks)
  deriving (Show) via (Quiet Entropic)

instance B.Buildable Entropic where
  build (Entropic n) = bprint (int . " entropic") n

instance Bounded Entropic where
  minBound = Entropic 0
  maxBound = Entropic maxEntropicVal

-- Used for debugging purposes only
instance ToJSON Entropic

instance ToCBOR Entropic where
  toCBOR = toCBOR . unsafeGetEntropic
  encodedSizeExpr size pxy = size (unsafeGetEntropic <$> pxy)

instance FromCBOR Entropic where
  fromCBOR = do
    l <- fromCBOR
    toCborError
      . first (DecoderErrorCustom "Entropic" . sformat build)
      $ mkEntropic l

instance Monad m => Canonical.ToJSON m Entropic where
  toJSON = Canonical.toJSON . unsafeGetEntropic

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m Entropic where
  fromJSON = fmap Entropic . Canonical.fromJSON

data EntropicError
  = EntropicOverflow Word64
  | EntropicTooLarge Integer
  | EntropicTooSmall Integer
  | EntropicUnderflow Word64 Word64
  deriving (Data, Eq, Show)

instance B.Buildable EntropicError where
  build = \case
    EntropicOverflow c ->
      bprint
        ("Entropic value, " . build . ", overflowed")
        c
    EntropicTooLarge c ->
      bprint
        ("Entropic value, " . build . ", exceeds maximum, " . build)
        c
        maxEntropicVal
    EntropicTooSmall c ->
      bprint
        ("Entropic value, " . build . ", is less than minimum, " . build)
        c
        (minBound :: Entropic)
    EntropicUnderflow c c' ->
      bprint
        ("Entropic underflow when subtracting " . build . " from " . build)
        c'
        c

instance ToCBOR EntropicError where
  toCBOR = \case
    EntropicOverflow c ->
      encodeListLen 2 <> toCBOR @Word8 0 <> toCBOR c
    EntropicTooLarge c ->
      encodeListLen 2 <> toCBOR @Word8 1 <> toCBOR c
    EntropicTooSmall c ->
      encodeListLen 2 <> toCBOR @Word8 2 <> toCBOR c
    EntropicUnderflow c c' ->
      encodeListLen 3 <> toCBOR @Word8 3 <> toCBOR c <> toCBOR c'

instance FromCBOR EntropicError where
  fromCBOR = do
    len <- decodeListLen
    let checkSize size = matchSize "EntropicError" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 2 >> EntropicOverflow <$> fromCBOR
      1 -> checkSize 2 >> EntropicTooLarge <$> fromCBOR
      2 -> checkSize 2 >> EntropicTooSmall <$> fromCBOR
      3 -> checkSize 3 >> EntropicUnderflow <$> fromCBOR <*> fromCBOR
      _ -> cborError $ DecoderErrorUnknownTag "TxValidationError" tag

-- | Maximal possible value of 'Entropic'
maxEntropicVal :: Word64
maxEntropicVal = 45e15

-- | Constructor for 'Entropic' returning 'EntropicError' when @c@ exceeds
--   'maxEntropicVal'
mkEntropic :: Word64 -> Either EntropicError Entropic
mkEntropic c
  | c <= maxEntropicVal = Right (Entropic c)
  | otherwise = Left (EntropicTooLarge (toInteger c))
{-# INLINE mkEntropic #-}

-- | Construct a 'Entropic' from a 'KnownNat', known to be less than
--   'maxEntropicVal'
mkKnownEntropic :: forall n. (KnownNat n, n <= 45000000000000000) => Entropic
mkKnownEntropic = Entropic . fromIntegral . natVal $ Proxy @n

-- | Entropic formatter which restricts type.
entropicF :: Format r (Entropic -> r)
entropicF = build

-- | Unwraps 'Entropic'. It's called “unsafe” so that people wouldn't use it
--   willy-nilly if they want to sum entropic or something. It's actually safe.
unsafeGetEntropic :: Entropic -> Word64
unsafeGetEntropic = unEntropic
{-# INLINE unsafeGetEntropic #-}

-- | Compute sum of all entropic in container. Result is 'Integer' as a
--   protection against possible overflow.
sumEntropic ::
  (Foldable t, Functor t) => t Entropic -> Either EntropicError Entropic
sumEntropic = integerToEntropic . sum . map entropicToInteger

entropicToInteger :: Entropic -> Integer
entropicToInteger = toInteger . unsafeGetEntropic
{-# INLINE entropicToInteger #-}

-- | Addition of entropic, returning 'EntropicError' in case of overflow
addEntropic :: Entropic -> Entropic -> Either EntropicError Entropic
addEntropic (Entropic a) (Entropic b)
  | res >= a && res >= b && res <= maxEntropicVal = Right (Entropic res)
  | otherwise = Left (EntropicOverflow res)
  where
    res = a + b
{-# INLINE addEntropic #-}

-- | Subtraction of entropic, returning 'EntropicError' on underflow
subEntropic :: Entropic -> Entropic -> Either EntropicError Entropic
subEntropic (Entropic a) (Entropic b)
  | a >= b = Right (Entropic (a - b))
  | otherwise = Left (EntropicUnderflow a b)

-- | Scale a 'Entropic' by an 'Integral' factor, returning 'EntropicError' when
--   the result is too large
scaleEntropic :: Integral b => Entropic -> b -> Either EntropicError Entropic
scaleEntropic (Entropic a) b = integerToEntropic $ toInteger a * toInteger b
{-# INLINE scaleEntropic #-}

-- | Scale a 'Entropic' by a rational factor, rounding down.
scaleEntropicRational :: Entropic -> Rational -> Entropic
scaleEntropicRational (Entropic a) b =
  Entropic $ fromInteger $ toInteger a * n `div` d
  where
    n, d :: Integer
    n = numerator b
    d = denominator b

-- | Scale a 'Entropic' by a rational factor, rounding up.
scaleEntropicRationalUp :: Entropic -> Rational -> Entropic
scaleEntropicRationalUp (Entropic a) b =
  Entropic $ fromInteger $ ceiling $ toRational a * b

-- | Integer division of a 'Entropic' by an 'Integral' factor
divEntropic :: Integral b => Entropic -> b -> Either EntropicError Entropic
divEntropic (Entropic a) b = integerToEntropic $ toInteger a `div` toInteger b
{-# INLINE divEntropic #-}

-- | Integer modulus of a 'Entropic' by an 'Integral' factor
modEntropic :: Integral b => Entropic -> b -> Either EntropicError Entropic
modEntropic (Entropic a) b = integerToEntropic $ toInteger a `mod` toInteger b
{-# INLINE modEntropic #-}

integerToEntropic :: Integer -> Either EntropicError Entropic
integerToEntropic n
  | n < 0 = Left (EntropicTooSmall n)
  | n <= entropicToInteger (maxBound :: Entropic) =
    Right $
      Entropic (fromInteger n)
  | otherwise = Left (EntropicTooLarge n)
