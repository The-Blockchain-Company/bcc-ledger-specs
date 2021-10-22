{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
-- Embed instances for (AurumEra TestCrypto)
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
nix-shell --arg config "{ haskellNix.profiling = true; }"
-}
-- Second set up the cabal.project.local (in the root of the bcc-ledger-specs directory) as follows.
{-
ignore-project: False
profiling: True
profiling-detail: all-functions
package zerepoch-core
  ghc-options: -fexternal-interpreter
-}
-- In the bcc-ledger-test directory (where the bcc-ledger-test.cabal file resides)
-- This cabal file defines the benchProperty benchmark. Now build with profiling enabled
--    cabal build benchProperty --enable-profiling
-- Now run the build with the -- +RTS -p   flags
--    cabal run benchProperty -- +RTS -p
--
-- When you are done be sure an reset the  cabal.project.local, and rebuild things
-- without profiling enabled. One way to do this is
-- Reset the cabal.project.local file
--  cabal configure
-- Rebuild everything in the current project
--  cabal build

-- | This benchmark file is for profiling Property tests. It appears as a benchmark
--     but we do not use any of the criterion stuff. We just run main, which is profiled.
-- First be sure and start the nix-shell with profiling enabled
module Main where

import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.PParams (PParams' (..))
import Bcc.Ledger.Aurum.Rules.Bbody (AurumBBODY)
import Bcc.Ledger.Aurum.Rules.Utxow (AurumUTXOW)
import Control.State.Transition.Extended (Embed (..))
import Sophie.Spec.Ledger.STS.Chain (CHAIN, ChainEvent (..), ChainPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Ledger (LEDGER, LedgerEvent (UtxowEvent), LedgerPredicateFailure (UtxowFailure))
import Test.Bcc.Ledger.Aurum.AurumEraGen ()
import Test.Bcc.Ledger.EraBuffet (TestCrypto)
import Test.Sophie.Spec.Ledger.Rules.ClassifyTraces (relevantCasesAreCovered)
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck

-- ===============================================================

instance Embed (AurumBBODY (AurumEra TestCrypto)) (CHAIN (AurumEra TestCrypto)) where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

instance Embed (AurumUTXOW (AurumEra TestCrypto)) (LEDGER (AurumEra TestCrypto)) where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

profileCover :: IO ()
profileCover =
  T.defaultMain $
    testProperty
      "Chain and Ledger traces cover the relevant cases"
      (withMaxSuccess 1 (relevantCasesAreCovered @(AurumEra TestCrypto)))

main :: IO ()
main = profileCover
