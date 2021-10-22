{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Sophie.Spec.Ledger.PParams (PParams' (..))
import Sophie.Spec.Ledger.STS.Ledger (LEDGER)
import Test.Bcc.Ledger.Evie.ScriptTranslation (testScriptPostTranslation)
import Test.Bcc.Ledger.Evie.Translation (evieTranslationTests)
import Test.Bcc.Ledger.EvieEraGen ()
import Test.Bcc.Ledger.EraBuffet (EvieEra, JenEra, TestCrypto)
import Test.Bcc.Ledger.Jen.Examples.MultiAssets (multiAssetsExample)
import Test.Bcc.Ledger.Jen.Golden (goldenScaledMinDeposit)
import Test.Bcc.Ledger.Jen.Translation (jenTranslationTests)
import Test.Bcc.Ledger.Jen.Value (valTests)
import Test.Bcc.Ledger.JenEraGen ()
import qualified Test.Bcc.Ledger.SophieMA.Serialisation as Serialisation
import Test.Sophie.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Tasty
import Test.Tasty.HUnit ()
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

type A = EvieEra TestCrypto

type AL = LEDGER (EvieEra TestCrypto)

type M = JenEra TestCrypto

type ML = LEDGER (JenEra TestCrypto)

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> mainTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "SophieMA Ledger Tests"
    [ evieTests,
      jenTests,
      testGroup
        "Mixed MA Ledger Tests"
        [ Serialisation.tests
        ]
    ]

evieTests :: TestTree
evieTests =
  testGroup
    "Evie Ledger Tests"
    [ evieTranslationTests,
      minimalPropertyTests @A @AL,
      testScriptPostTranslation
    ]

jenTests :: TestTree
jenTests =
  testGroup
    "Jen Ledger Tests"
    [ jenTranslationTests,
      valTests,
      multiAssetsExample,
      goldenScaledMinDeposit
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "SophieMA Ledger - nightly"
    [ testGroup
        "Evie Ledger - nightly"
        [ propertyTests @A @AL
        ],
      testGroup
        "Jen Ledger - nightly"
        [ propertyTests @M @ML
        ]
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
