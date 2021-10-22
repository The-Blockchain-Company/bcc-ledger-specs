{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Bcc.Ledger.Example (ExampleEra)
import Sophie.Spec.Ledger.PParams (PParams' (..))
import Sophie.Spec.Ledger.RewardUpdate ()
import Sophie.Spec.Ledger.STS.Ledger (LEDGER)
import Test.Bcc.Ledger.Example ()
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Sophie.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Tasty
import Test.Tasty.HUnit ()
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

type E = ExampleEra C_Crypto

type L = LEDGER E

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> mainTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Example Consensus Tests"
    [ minimalPropertyTests @E @L
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Example Consensus - nightly"
    [ testGroup
        "Example Era - nightly"
        [ propertyTests @E @L
        ]
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
