{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Crypto.Json
  ( tests,
  )
where

import Bcc.Prelude
import Hedgehog (Property)
import qualified Hedgehog as H
import Test.Bcc.Crypto.Example
  ( exampleProtocolMagic3,
    exampleProtocolMagic4,
  )
import Test.Bcc.Prelude

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

-- Legacy JSON encoding where requiresNetworkMagic was
-- encoded as "NMMustBeNothing" or "NMMustBeJust"

goldenProtocolMagic3AesonDec_NMMustBeJust :: Property
goldenProtocolMagic3AesonDec_NMMustBeJust =
  goldenTestJSONDec
    exampleProtocolMagic3
    "test/golden/json/ProtocolMagic_Legacy_NMMustBeJust"

goldenProtocolMagic4AesonDec_NMMustBeNothing :: Property
goldenProtocolMagic4AesonDec_NMMustBeNothing =
  goldenTestJSONDec
    exampleProtocolMagic4
    "test/golden/json/ProtocolMagic_Legacy_NMMustBeNothing"

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
