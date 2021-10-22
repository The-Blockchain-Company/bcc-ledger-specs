-- | Benchmarks for the serialisation generators
module Bench.Bcc.Ledger.Serialisation.Generators
  ( benchTxGeneration,
  )
where

import Criterion.Main
import Sophie.Spec.Ledger.Tx
import Test.Bcc.Ledger.EraBuffet
import Test.Bcc.Ledger.SophieMA.Serialisation.Generators ()
import Test.QuickCheck

genTxSophie :: IO (Tx (SophieEra TestCrypto))
genTxSophie = generate arbitrary

-- | Generate an arbitrary Evie transaction
genTxEvie :: IO (Tx (EvieEra TestCrypto))
genTxEvie = generate arbitrary

-- | Generate an arbitrary Jen transaction
genTxJen :: IO (Tx (JenEra TestCrypto))
genTxJen = generate arbitrary

benchTxGeneration :: Benchmark
benchTxGeneration =
  bgroup
    "txgen"
    [ bench "genTxSophie" (whnfIO genTxSophie),
      bench "genTxEvie" (whnfIO genTxEvie),
      bench "genTxJen" (whnfIO genTxJen)
    ]
