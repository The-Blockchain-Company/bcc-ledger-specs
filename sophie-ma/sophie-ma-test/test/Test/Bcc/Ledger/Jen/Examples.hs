{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Jen.Examples
  ( testJenNoDelegLEDGER,
  )
where

-- obtaining orphan STS (UTXOW (SophieMAEra ma c))

import Bcc.Ledger.Jen (JenEra)
import Bcc.Ledger.SophieMA.Rules.Utxow ()
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Data.Default.Class (def)
import GHC.Records
import Sophie.Spec.Ledger.API (LEDGER, LedgerEnv (..))
import Sophie.Spec.Ledger.LedgerState
  ( DPState (..),
    UTxOState (..),
  )
import Sophie.Spec.Ledger.PParams (PParams' (..))
import Sophie.Spec.Ledger.Tx (Tx (..))
import Sophie.Spec.Ledger.UTxO (UTxO)
import Test.Bcc.Ledger.EraBuffet (TestCrypto)
import Test.Sophie.Spec.Ledger.VestedSealUtils (applySTSTest, runSophieBase)
import Test.Tasty.HUnit (Assertion, (@?=))

type JenTest = JenEra TestCrypto

ignoreAllButUTxO ::
  Either [PredicateFailure (LEDGER JenTest)] (UTxOState JenTest, DPState TestCrypto) ->
  Either [PredicateFailure (LEDGER JenTest)] (UTxO JenTest)
ignoreAllButUTxO = fmap (\(UTxOState utxo _ _ _, _) -> utxo)

testJenNoDelegLEDGER ::
  UTxO JenTest ->
  Tx JenTest ->
  LedgerEnv JenTest ->
  Either [PredicateFailure (LEDGER JenTest)] (UTxO JenTest) ->
  Assertion
testJenNoDelegLEDGER utxo tx env (Right expectedUTxO) = do
  checkTrace @(LEDGER JenTest) runSophieBase env $
    pure (def {_utxo = utxo}, def) .- tx .-> expectedSt'
  where
    txFee = getField @"txfee" (getField @"body" tx)
    expectedSt' = (def {_utxo = expectedUTxO, _fees = txFee}, def)
testJenNoDelegLEDGER utxo tx env predicateFailure@(Left _) = do
  let st = runSophieBase $ applySTSTest @(LEDGER JenTest) (TRC (env, (def {_utxo = utxo}, def), tx))
  ignoreAllButUTxO st @?= predicateFailure
