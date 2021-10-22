{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Bcc.Ledger.Aurum.Examples (zerepochScriptExamples)
import Test.Bcc.Ledger.Aurum.Golden as Golden
import qualified Test.Bcc.Ledger.Aurum.Serialisation.CDDL as CDDL
import qualified Test.Bcc.Ledger.Aurum.Serialisation.Canonical as Canonical
import qualified Test.Bcc.Ledger.Aurum.Serialisation.Tripping as Tripping
import qualified Test.Bcc.Ledger.Aurum.Translation as Translation
import Test.Bcc.Ledger.Aurum.Trials (aurumPropertyTests, fastPropertyTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

-- ====================================================================================

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Aurum tests"
    [ fastPropertyTests, -- These are still pretty slow (it is just that a few are omitted)
      Tripping.tests,
      Translation.tests,
      Canonical.tests,
      CDDL.tests 5,
      Golden.goldenUTxOEntryMinBcc,
      zerepochScriptExamples
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Aurum tests"
    [ Tripping.tests,
      Translation.tests,
      CDDL.tests 1,
      Golden.goldenUTxOEntryMinBcc,
      zerepochScriptExamples
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Aurum tests"
    [ aurumPropertyTests, -- These are the full property tests
      CDDL.tests 50
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
