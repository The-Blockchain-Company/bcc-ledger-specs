{-# LANGUAGE TypeApplications #-}

module Test.Sophie.Spec.Ledger.Examples
  ( CHAINExample (..),
    testCHAINExample,
  )
where

import Bcc.Ledger.Sophie ()
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Sophie.Spec.Ledger.BlockChain (Block)
import Sophie.Spec.Ledger.PParams (PParams' (..))
import Sophie.Spec.Ledger.STS.Chain (CHAIN, ChainState, totalBcc)
import Sophie.Spec.Ledger.Scripts ()
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Sophie.Spec.Ledger.Orphans ()
import Test.Sophie.Spec.Ledger.VestedSealUtils (applySTSTest, maxLLSupply, runSophieBase)
import Test.Tasty.HUnit (Assertion, (@?=))

data CHAINExample h = CHAINExample
  { -- | State to start testing with
    startState :: ChainState h,
    -- | Block to run chain state transition system on
    newBlock :: Block h,
    -- | type of fatal error, if failure expected and final chain state if success expected
    intendedResult :: Either [PredicateFailure (CHAIN h)] (ChainState h)
  }

-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: CHAINExample C -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  (checkTrace @(CHAIN C) runSophieBase () $ pure initSt .- block .-> expectedSt)
    >> (totalBcc expectedSt @?= maxLLSupply)
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runSophieBase $ applySTSTest @(CHAIN C) (TRC ((), initSt, block))
  st @?= predicateFailure
