module Main
  ( main,
  )
where

import Bcc.Prelude
import qualified Test.Bcc.Chain.Block.CBOR
import qualified Test.Bcc.Chain.Block.Model
import qualified Test.Bcc.Chain.Block.Size
import qualified Test.Bcc.Chain.Block.Validation
import qualified Test.Bcc.Chain.Block.ValidationMode
import qualified Test.Bcc.Chain.Buildable
import qualified Test.Bcc.Chain.Cole.API
import qualified Test.Bcc.Chain.Common.Address
import qualified Test.Bcc.Chain.Common.CBOR
import qualified Test.Bcc.Chain.Common.Compact
import qualified Test.Bcc.Chain.Common.Entropic
import qualified Test.Bcc.Chain.Delegation.CBOR
import qualified Test.Bcc.Chain.Delegation.Certificate
import qualified Test.Bcc.Chain.Delegation.Model
import qualified Test.Bcc.Chain.Elaboration.Delegation
import qualified Test.Bcc.Chain.Epoch.File
import qualified Test.Bcc.Chain.Genesis.CBOR
import qualified Test.Bcc.Chain.Genesis.Json
import qualified Test.Bcc.Chain.MempoolPayload.CBOR
import qualified Test.Bcc.Chain.Slotting.CBOR
import qualified Test.Bcc.Chain.Slotting.Properties
import qualified Test.Bcc.Chain.Ssc.CBOR
import qualified Test.Bcc.Chain.UTxO.CBOR
import qualified Test.Bcc.Chain.UTxO.Compact
import qualified Test.Bcc.Chain.UTxO.Model
import qualified Test.Bcc.Chain.UTxO.ValidationMode
import qualified Test.Bcc.Chain.Update.CBOR
import qualified Test.Bcc.Chain.Update.Properties
import Test.Options (ShouldAssertNF (..), mainWithTestScenario, tsGroupToTree)
import Test.Tasty (testGroup)

main :: IO ()
main =
  mainWithTestScenario $
    testGroup "Bcc Ledger Tests" $
      tsGroupToTree
        <$> [ Test.Bcc.Chain.Block.CBOR.tests,
              Test.Bcc.Chain.Block.Model.tests,
              Test.Bcc.Chain.Block.Size.tests,
              Test.Bcc.Chain.Block.Validation.tests NoAssertNF,
              Test.Bcc.Chain.Block.ValidationMode.tests,
              Test.Bcc.Chain.Buildable.tests,
              Test.Bcc.Chain.Common.Address.tests,
              Test.Bcc.Chain.Common.CBOR.tests,
              Test.Bcc.Chain.Common.Compact.tests,
              Test.Bcc.Chain.Common.Entropic.tests,
              Test.Bcc.Chain.Delegation.CBOR.tests,
              const Test.Bcc.Chain.Delegation.Certificate.tests,
              const Test.Bcc.Chain.Delegation.Model.tests,
              const Test.Bcc.Chain.Epoch.File.tests,
              Test.Bcc.Chain.Elaboration.Delegation.tests,
              Test.Bcc.Chain.Genesis.CBOR.tests,
              Test.Bcc.Chain.Genesis.Json.tests,
              Test.Bcc.Chain.MempoolPayload.CBOR.tests,
              Test.Bcc.Chain.Slotting.CBOR.tests,
              Test.Bcc.Chain.Slotting.Properties.tests,
              const Test.Bcc.Chain.Ssc.CBOR.tests,
              Test.Bcc.Chain.UTxO.CBOR.tests,
              Test.Bcc.Chain.UTxO.Compact.tests,
              Test.Bcc.Chain.UTxO.Model.tests,
              Test.Bcc.Chain.UTxO.ValidationMode.tests,
              Test.Bcc.Chain.Update.CBOR.tests,
              Test.Bcc.Chain.Update.Properties.tests,
              Test.Bcc.Chain.Cole.API.tests
            ]
