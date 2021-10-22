module Test.Sophie.Spec.Ledger.Serialisation where

import qualified Test.Sophie.Spec.Ledger.Serialisation.CDDL
import qualified Test.Sophie.Spec.Ledger.Serialisation.Golden.Address
import qualified Test.Sophie.Spec.Ledger.Serialisation.Golden.Encoding
import qualified Test.Sophie.Spec.Ledger.Serialisation.Golden.Genesis
import qualified Test.Sophie.Spec.Ledger.Serialisation.Tripping.CBOR
import qualified Test.Sophie.Spec.Ledger.Serialisation.Tripping.JSON
import Test.Tasty

tests :: Int -> TestTree
tests cnt =
  testGroup
    "Serialisation tests"
    [ Test.Sophie.Spec.Ledger.Serialisation.Golden.Address.tests,
      Test.Sophie.Spec.Ledger.Serialisation.Golden.Encoding.tests,
      Test.Sophie.Spec.Ledger.Serialisation.Golden.Genesis.tests,
      Test.Sophie.Spec.Ledger.Serialisation.Tripping.CBOR.tests,
      Test.Sophie.Spec.Ledger.Serialisation.Tripping.JSON.tests,
      Test.Sophie.Spec.Ledger.Serialisation.CDDL.tests cnt
    ]
