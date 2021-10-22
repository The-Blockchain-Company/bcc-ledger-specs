{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Bcc.Chain.Common.TxSizeLinear
  ( TxSizeLinear (..),
    txSizeLinearMinValue,
    calculateTxSizeLinear,
  )
where

import Bcc.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
  )
import Bcc.Chain.Common.Entropic
  ( Entropic,
    EntropicError,
    addEntropic,
    integerToEntropic,
    mkEntropic,
    scaleEntropicRationalUp,
    unsafeGetEntropic,
  )
import Bcc.Prelude
import Data.Aeson (ToJSON)
import Data.Fixed (Nano)
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear
  = TxSizeLinear !Entropic !Rational
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance B.Buildable TxSizeLinear where
  build (TxSizeLinear a b) = bprint (build . " + " . build . "*s") a b

-- Used for debugging purposes only
instance ToJSON TxSizeLinear

instance ToCBOR TxSizeLinear where
  -- We encode as 'Nano' for backwards compatibility
  toCBOR (TxSizeLinear a b) =
    encodeListLen 2
      <> toCBOR (fromIntegral (unsafeGetEntropic a) :: Nano)
      <> toCBOR (fromRational b :: Nano)

instance FromCBOR TxSizeLinear where
  fromCBOR = do
    enforceSize "TxSizeLinear" 2
    !a <- wrapEntropicError . mkEntropic . round =<< fromCBOR @Nano
    !b <- toRational <$> fromCBOR @Nano
    return $ TxSizeLinear a b
    where
      wrapEntropicError :: Either EntropicError Entropic -> Decoder s Entropic
      wrapEntropicError =
        toCborError . first (DecoderErrorCustom "TxSizeLinear" . sformat build)

calculateTxSizeLinear ::
  TxSizeLinear -> Natural -> Either EntropicError Entropic
calculateTxSizeLinear (TxSizeLinear a b) sz =
  addEntropic a
    =<< flip scaleEntropicRationalUp b
    <$> integerToEntropic (fromIntegral sz)

txSizeLinearMinValue :: TxSizeLinear -> Entropic
txSizeLinearMinValue (TxSizeLinear a _) = a
