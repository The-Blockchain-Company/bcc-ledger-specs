{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Chain.Delegation.CBOR
  ( tests,
  )
where

import Bcc.Chain.Delegation (unsafePayload)
import Bcc.Prelude
import Data.List ((!!))
import Hedgehog (Property)
import Test.Bcc.Binary.Helpers.GoldenRoundTrip
  ( goldenTestCBOR,
    roundTripsCBORBuildable,
    roundTripsCBORShow,
  )
import Test.Bcc.Chain.Delegation.Example (exampleCertificates)
import Test.Bcc.Chain.Delegation.Gen
  ( genCertificate,
    genError,
    genPayload,
  )
import Test.Bcc.Crypto.Gen (feedPM)
import Test.Bcc.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- Certificate
--------------------------------------------------------------------------------

goldenCertificate :: Property
goldenCertificate =
  goldenTestCBOR
    cert
    "test/golden/cbor/delegation/Certificate"
  where
    cert = exampleCertificates !! 0

ts_roundTripCertificateCBOR :: TSProperty
ts_roundTripCertificateCBOR =
  eachOfTS 200 (feedPM genCertificate) roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------

goldenDlgPayload :: Property
goldenDlgPayload = goldenTestCBOR dp "test/golden/cbor/delegation/DlgPayload"
  where
    dp = unsafePayload (take 4 exampleCertificates)

ts_roundTripDlgPayloadCBOR :: TSProperty
ts_roundTripDlgPayloadCBOR =
  eachOfTS 100 (feedPM genPayload) roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

ts_roundTripErrorCBOR :: TSProperty
ts_roundTripErrorCBOR =
  eachOfTS 100 genError roundTripsCBORShow

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
