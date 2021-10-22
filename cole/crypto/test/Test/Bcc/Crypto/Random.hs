{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Crypto.Random
  ( tests,
  )
where

import Bcc.Crypto.Random (deterministic, randomNumber)
import Bcc.Prelude
import qualified Data.ByteString as BS
import Hedgehog (Property, checkParallel, discover, property, withTests, (===))

tests :: IO Bool
tests = checkParallel $$discover

prop_randomNumberDeterminism :: Property
prop_randomNumberDeterminism = withTests 1 . property $ do
  let seed = BS.pack [1 .. 40]
  deterministic seed (randomNumber 1) === 0
