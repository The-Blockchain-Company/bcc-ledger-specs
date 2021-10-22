module Test.Bcc.Ledger.SophieMA.Serialisation where

import Test.Bcc.Ledger.Evie.Translation (evieEncodeDecodeTests)
import Test.Bcc.Ledger.Jen.Translation (jenEncodeDecodeTests)
import Test.Bcc.Ledger.SophieMA.Serialisation.CDDL (cddlTests)
import Test.Bcc.Ledger.SophieMA.Serialisation.Coders (codersTest)
import Test.Bcc.Ledger.SophieMA.Serialisation.Golden.Encoding (goldenEncodingTests)
import Test.Bcc.Ledger.SophieMA.Serialisation.Roundtrip (allEraRoundtripTests)
import Test.Bcc.Ledger.SophieMA.Serialisation.Timelocks (timelockTests)
import Test.Bcc.Ledger.SophieMA.TxBody (txBodyTest)
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Serialisation tests"
    [ codersTest,
      evieEncodeDecodeTests,
      jenEncodeDecodeTests,
      txBodyTest,
      timelockTests,
      cddlTests 10,
      goldenEncodingTests,
      allEraRoundtripTests
    ]
