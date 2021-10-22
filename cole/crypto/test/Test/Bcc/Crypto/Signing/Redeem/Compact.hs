{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Crypto.Signing.Redeem.Compact
  ( tests,
  )
where

import Bcc.Crypto.Signing.Redeem
  ( fromCompactRedeemVerificationKey,
    toCompactRedeemVerificationKey,
  )
import Bcc.Prelude
import Hedgehog (MonadTest, Property, checkParallel, tripping)
import Test.Bcc.Crypto.Gen (genRedeemVerificationKey)
import Test.Bcc.Prelude

--------------------------------------------------------------------------------
-- Compact RedeemVerificationKey
--------------------------------------------------------------------------------

roundTripCompactRedeemVerificationKey :: Property
roundTripCompactRedeemVerificationKey =
  eachOf
    1000
    genRedeemVerificationKey
    (trippingCompact toCompactRedeemVerificationKey fromCompactRedeemVerificationKey)

-------------------------------------------------------------------------------
-- Tripping util
-------------------------------------------------------------------------------

trippingCompact ::
  (HasCallStack, MonadTest m, Show a, Show b, Eq a) =>
  (a -> b) ->
  (b -> a) ->
  a ->
  m ()
trippingCompact toCompact fromCompact x =
  tripping x toCompact (Identity . fromCompact)

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discoverRoundTrip
