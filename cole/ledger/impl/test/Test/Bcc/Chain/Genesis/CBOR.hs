{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Chain.Genesis.CBOR
  ( tests,
  )
where

import Bcc.Prelude
import Test.Bcc.Binary.Helpers.GoldenRoundTrip (roundTripsCBORBuildable, roundTripsCBORShow)
import Test.Bcc.Chain.Genesis.Gen
import Test.Bcc.Crypto.Gen (genProtocolMagicId)
import Test.Bcc.Prelude (discoverRoundTripArg)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- GenesisKeyHash
--------------------------------------------------------------------------------
ts_roundTripGenesisKeyHashesCBOR :: TSProperty
ts_roundTripGenesisKeyHashesCBOR =
  eachOfTS 1000 genGenesisKeyHashes roundTripsCBORBuildable

tests :: TSGroup
tests =
  concatTSGroups
    [$$discoverRoundTripArg]

--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------
ts_roundTripGenesisAvvmBalances :: TSProperty
ts_roundTripGenesisAvvmBalances =
  eachOfTS 1000 genGenesisAvvmBalances roundTripsCBORShow

--------------------------------------------------------------------------------
-- GenesisNonAvvmBalances
--------------------------------------------------------------------------------
ts_roundTripGenesisNonAvvmBalances :: TSProperty
ts_roundTripGenesisNonAvvmBalances =
  eachOfTS 1000 genGenesisNonAvvmBalances roundTripsCBORShow

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------
ts_roundTripGenesisDelegation :: TSProperty
ts_roundTripGenesisDelegation =
  eachOfTS 1000 (genGenesisDelegation =<< genProtocolMagicId) roundTripsCBORShow

--------------------------------------------------------------------------------
-- GenesisData
--------------------------------------------------------------------------------
ts_roundTripGenesisData :: TSProperty
ts_roundTripGenesisData =
  eachOfTS 1000 (genConfig =<< genProtocolMagicId) roundTripsCBORShow
