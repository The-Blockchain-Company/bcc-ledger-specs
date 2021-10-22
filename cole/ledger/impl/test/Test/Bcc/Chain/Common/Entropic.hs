{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Chain.Common.Entropic
  ( tests,
  )
where

import Bcc.Chain.Common
  ( EntropicError (..),
    addEntropic,
    integerToEntropic,
    maxEntropicVal,
    mkKnownEntropic,
    mkEntropic,
    scaleEntropic,
    subEntropic,
    unsafeGetEntropic,
  )
import Bcc.Prelude
import Data.Data (Constr, toConstr)
import Formatting (build, sformat)
import Hedgehog (Property, discover, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Bcc.Chain.Common.Gen (genCustomEntropic, genEntropic)
import Test.Bcc.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, withTestsTS)

ts_prop_addEntropic :: TSProperty
ts_prop_addEntropic = withTestsTS 1000 . property $ do
  a <- forAll genEntropic
  let newRange = maxEntropicVal - unsafeGetEntropic a
  b <- forAll $ genCustomEntropic newRange
  assertIsRight $ addEntropic a b

prop_addEntropicOverflow :: Property
prop_addEntropicOverflow =
  property $
    assertIsLeftConstr
      dummyEntropicOverflow
      (addEntropic (mkKnownEntropic @1) maxBound)

ts_prop_integerToEntropic :: TSProperty
ts_prop_integerToEntropic = withTestsTS 1000 . property $ do
  testInt <-
    forAll
      (Gen.integral $ Range.linear 0 (fromIntegral maxEntropicVal :: Integer))
  assertIsRight $ integerToEntropic testInt

prop_integerToEntropicTooLarge :: Property
prop_integerToEntropicTooLarge =
  property $
    assertIsLeftConstr
      dummyEntropicTooLarge
      (integerToEntropic (fromIntegral (maxEntropicVal + 1) :: Integer))

prop_integerToEntropicTooSmall :: Property
prop_integerToEntropicTooSmall =
  property $
    assertIsLeftConstr dummyEntropicTooSmall (integerToEntropic (negate 1))

prop_maxEntropicUnchanged :: Property
prop_maxEntropicUnchanged =
  property $ (fromIntegral maxEntropicVal :: Integer) === 45e15

ts_prop_mkEntropic :: TSProperty
ts_prop_mkEntropic = withTestsTS 1000 . property $ do
  testWrd <- forAll (Gen.word64 $ Range.linear 0 maxEntropicVal)
  assertIsRight $ mkEntropic testWrd

prop_mkEntropicTooLarge :: Property
prop_mkEntropicTooLarge =
  property $
    assertIsLeftConstr dummyEntropicTooLarge (mkEntropic (maxEntropicVal + 1))

prop_scaleEntropicTooLarge :: Property
prop_scaleEntropicTooLarge =
  property $
    assertIsLeftConstr
      dummyEntropicTooLarge
      (scaleEntropic maxBound (2 :: Integer))

ts_prop_subEntropic :: TSProperty
ts_prop_subEntropic = withTestsTS 1000 . property $ do
  a <- forAll genEntropic
  b <- forAll $ genCustomEntropic (unsafeGetEntropic a)
  assertIsRight $ subEntropic a b

ts_prop_subEntropicUnderflow :: TSProperty
ts_prop_subEntropicUnderflow =
  withTestsTS 1000
    . property
    $ do
      -- (maxEntropicVal - 1) to avoid an overflow error in `addEntropic`
      -- in case expression
      a <- forAll $ genCustomEntropic (maxEntropicVal - 1)
      case addEntropic a (mkKnownEntropic @1) of
        Right added ->
          assertIsLeftConstr dummyEntropicUnderflow (subEntropic a added)
        Left err ->
          panic $
            sformat
              ("The impossible happened in subEntropicUnderflow: " . build)
              err

tests :: TSGroup
tests = concatTSGroups [const $$discover, $$discoverPropArg]

--------------------------------------------------------------------------------
-- Dummy values for constructor comparison in assertIsLeftConstr tests
--------------------------------------------------------------------------------

dummyEntropicOverflow :: Constr
dummyEntropicOverflow = toConstr $ EntropicOverflow 1

dummyEntropicTooLarge :: Constr
dummyEntropicTooLarge = toConstr $ EntropicTooLarge 1

dummyEntropicTooSmall :: Constr
dummyEntropicTooSmall = toConstr $ EntropicTooSmall 1

dummyEntropicUnderflow :: Constr
dummyEntropicUnderflow = toConstr $ EntropicUnderflow 1 1
