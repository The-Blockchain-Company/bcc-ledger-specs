{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Module containing the imports needed to test that a given abstract trace
-- passes the concrete validation. This is useful when debugging
-- counterexamples.
--
-- Usage from the ghci repl:
--
-- > import Hedgehog
-- > Hedgehog.check $ property $ passConcreteValidation trace0
--
-- Replace @trace0@ by the trace under analysis.
module Test.Bcc.Chain.Block.Model.Examples where

import Cole.Spec.Chain.STS.Block
import Cole.Spec.Chain.STS.Rule.Chain
import Cole.Spec.Ledger.Core
import Cole.Spec.Ledger.Delegation
import Cole.Spec.Ledger.STS.UTXO
import Cole.Spec.Ledger.UTxO
import Cole.Spec.Ledger.Update
import Bcc.Prelude hiding (State, trace)
import Control.State.Transition
import Control.State.Transition.Trace
import GHC.Exts

-- | A trace example. When debugging conformance tests you can add such a
-- trace, and use it in the repl. __NOTE__: Do not commit such trace.
trace0 :: Trace CHAIN
trace0 = mkTrace traceEnv0 traceInitState0 traceTrans0
  where
    traceEnv0 :: Environment CHAIN
    traceEnv0 = panic "Add the trace environment here."

    traceInitState0 :: State CHAIN
    traceInitState0 = panic "Add the trace initial state here."

    traceTrans0 :: [(State CHAIN, Signal CHAIN)]
    traceTrans0 = panic "Add the trace transitions here."
