{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Chain.Genesis.Json
  ( tests,
  )
where

import Bcc.Prelude
import Hedgehog (Property)
import Test.Bcc.Chain.Delegation.Gen (genCanonicalCertificate)
import Test.Bcc.Chain.Genesis.Example (exampleGenesisData0)
import Test.Bcc.Chain.Genesis.Gen
  ( genCanonicalGenesisData,
    genCanonicalGenesisDelegation,
    genGenesisAvvmBalances,
    genGenesisKeyHashes,
    genGenesisNonAvvmBalances,
  )
import Test.Bcc.Chain.Update.Gen
  ( genCanonicalProtocolParameters,
  )
import Test.Bcc.Crypto.Gen (feedPM)
import Test.Bcc.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- JSON Canonical Tests
--------------------------------------------------------------------------------

ts_roundTripCanonicalCertificate :: TSProperty
ts_roundTripCanonicalCertificate =
  eachOfTS 100 (feedPM genCanonicalCertificate) roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisAvvmBalances :: TSProperty
ts_roundTripCanonicalGenesisAvvmBalances =
  eachOfTS 100 genGenesisAvvmBalances roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisData :: TSProperty
ts_roundTripCanonicalGenesisData =
  eachOfTS 100 (feedPM genCanonicalGenesisData) roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisDelegation :: TSProperty
ts_roundTripCanonicalGenesisDelegation =
  eachOfTS 100 (feedPM genCanonicalGenesisDelegation) roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisNonAvvmBalances :: TSProperty
ts_roundTripCanonicalGenesisNonAvvmBalances =
  eachOfTS 100 genGenesisNonAvvmBalances roundTripsCanonicalJsonPretty

ts_roundTripCanonicalGenesisKeyHashes :: TSProperty
ts_roundTripCanonicalGenesisKeyHashes =
  eachOfTS 100 genGenesisKeyHashes roundTripsCanonicalJsonPretty

ts_roundTripCanonicalProtocolParameters :: TSProperty
ts_roundTripCanonicalProtocolParameters =
  eachOfTS 100 genCanonicalProtocolParameters roundTripsCanonicalJsonPretty

--------------------------------------------------------------------------------
-- GenesisData (Canonical JSON)
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `GenesisData` canonical JSON format, the `RequiresNetworkMagic` field
-- defaults to `RequiresMagic`.

golden_GenesisData0Dec :: Property
golden_GenesisData0Dec =
  goldenTestCanonicalJSONDec
    exampleGenesisData0
    "test/golden/json/genesis/GenesisData0_Legacy_HasNetworkMagic"

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverPropArg]
