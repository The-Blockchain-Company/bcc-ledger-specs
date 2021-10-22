This directory contains sample encoded resources for benchmarking purposes. They
are indexed by number.

# Example 0

Sophie era ledger state and transaction for benchmarking of transaction
application.

## Generation
These examples were generated using the following script (executed in `cabal repl sophie-spec-ledger-test`)

```
:set -XTypeApplications

import Data.Proxy

import Bcc.Binary
import Control.State.Transition.Trace
import Control.State.Transition.Trace.Generator.QuickCheck
import qualified Data.ByteString as BS
import Sophie.Spec.Ledger.STS.Ledger
import Bcc.Ledger.Sophie
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes

import Test.Sophie.Spec.Ledger.SentryUtils
import Test.Sophie.Spec.Ledger.Generator.Presets
import Test.Sophie.Spec.Ledger.Generator.Trace.Ledger ()
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Sophie.Spec.Ledger.PParams (PParams'(..))

import Test.QuickCheck (generate)

import Test.Sophie.Spec.Ledger.Generator.Trace.Ledger

let ge = genEnv (Proxy @(SophieEra C_Crypto))
initLs <- generate $ mkGenesisLedgerState @([[LedgerPredicateFailure (SophieEra C_Crypto)]]) ge undefined
tr <- generate $ traceFromInitState @(LEDGER (SophieEra C_Crypto)) testGlobals 20 ge (Just $ \_ -> pure initLs)

let sst = last $ sourceSignalTargets tr
BS.writeFile "/tmp/0_ledgerstate/cbor" $ serialize' (source sst)
BS.writeFile "/tmp/0_ledgerstate/cbor" $ serialize' (signal sst)
```
