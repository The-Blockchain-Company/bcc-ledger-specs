{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.SophieMA.Serialisation.Roundtrip where

import Bcc.Binary (Annotator (..), FromCBOR, ToCBOR)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Sophie.Constraints (SophieBased)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import Sophie.Spec.Ledger.API (ApplyTx, ApplyTxError)
import Test.Bcc.Ledger.EraBuffet
import Test.Bcc.Ledger.SophieMA.Serialisation.Coders
  ( roundTrip,
    roundTripAnn,
  )
import Test.Bcc.Ledger.SophieMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary, Property, counterexample, (===))
import Test.Sophie.Spec.Ledger.Generator.Metadata ()
import Test.Sophie.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

propertyAnn ::
  forall t.
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR (Annotator t)
  ) =>
  t ->
  Property
propertyAnn x = case roundTripAnn x of
  Right (remaining, y) | BSL.null remaining -> x === y
  Right (remaining, _) ->
    counterexample
      ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
      False
  Left stuff ->
    counterexample
      ("Failed to decode: " <> show stuff)
      False

property ::
  forall t.
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR t
  ) =>
  t ->
  Property
property x =
  case roundTrip x of
    Right (remaining, y) | BSL.null remaining -> x === y
    Right (remaining, _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
    Left stuff ->
      counterexample
        ("Failed to decode: " <> show stuff)
        False

allprops ::
  forall e.
  ( SophieBased e,
    ApplyTx e,
    Arbitrary (Core.TxBody e),
    Arbitrary (Core.AuxiliaryData e),
    Arbitrary (Core.Value e),
    Arbitrary (Core.Script e),
    Arbitrary (ApplyTxError e)
  ) =>
  TestTree
allprops =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "TxBody" $ propertyAnn @(Core.TxBody e),
      testProperty "Metadata" $ propertyAnn @(Core.AuxiliaryData e),
      testProperty "Value" $ property @(Core.Value e),
      testProperty "Script" $ propertyAnn @(Core.Script e),
      testProperty "ApplyTxError" $ property @(ApplyTxError e)
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [ allprops @(SophieEra StandardCrypto),
      allprops @(EvieEra StandardCrypto),
      allprops @(JenEra StandardCrypto)
    ]
