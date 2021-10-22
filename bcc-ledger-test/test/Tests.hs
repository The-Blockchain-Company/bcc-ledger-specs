{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Test.Bcc.Ledger.Aurum.Tools as Tools
import Test.Bcc.Ledger.BaseTypes (baseTypesTests)
import Test.Bcc.Ledger.Examples.TwoPhaseValidation
  ( aurumAPITests,
    aurumBBODYexamples,
    aurumUTXOWexamples,
    collectOrderingAurum,
  )
import Test.Bcc.Ledger.Properties (aurumProperties)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

-- ====================================================================================

tests :: TestTree
tests = askOption $ \case
  Nightly -> mainTests
  Fast -> mainTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "bcc-core"
    [ baseTypesTests,
      Tools.tests,
      testGroup
        "STS Tests"
        [ aurumUTXOWexamples,
          aurumBBODYexamples,
          aurumAPITests,
          collectOrderingAurum,
          aurumProperties
        ]
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
