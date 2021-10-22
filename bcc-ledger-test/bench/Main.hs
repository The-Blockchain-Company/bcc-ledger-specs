{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This benchmark file is a placholder for benchmarks
module Main where

import qualified Bench.Bcc.Ledger.ApplyTx as ApplyTx
import qualified Bench.Bcc.Ledger.EpochBoundary as Epoch
import qualified Bench.Bcc.Ledger.Serialisation.Generators as SerGen
import Criterion.Main
  ( -- bench, bgroup, nf,
    defaultMain,
  )

main :: IO ()
main =
  defaultMain
    [ SerGen.benchTxGeneration,
      ApplyTx.applyTxBenchmarks,
      Epoch.aggregateUtxoBench
    ]
