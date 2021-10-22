{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Chain.Slotting.CBOR
  ( tests,
  )
where

import Bcc.Chain.Slotting (EpochSlots (..), SlotNumber)
import Bcc.Prelude
import Hedgehog (Property)
import Test.Bcc.Binary.Helpers.GoldenRoundTrip
  ( goldenTestCBOR,
    roundTripsCBORBuildable,
  )
import Test.Bcc.Chain.Slotting.Example
  ( exampleEpochAndSlotCount,
    exampleEpochNumber,
  )
import Test.Bcc.Chain.Slotting.Gen
  ( feedPMEpochSlots,
    genEpochAndSlotCount,
    genEpochNumber,
    genEpochSlots,
    genSlotNumber,
  )
import Test.Bcc.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- EpochNumber
--------------------------------------------------------------------------------
golden_EpochNumber :: Property
golden_EpochNumber =
  goldenTestCBOR exampleEpochNumber "test/golden/cbor/slotting/EpochNumber"

ts_roundTripEpochNumberCBOR :: TSProperty
ts_roundTripEpochNumberCBOR = eachOfTS 1000 genEpochNumber roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- SlotNumber
--------------------------------------------------------------------------------
golden_SlotNumber :: Property
golden_SlotNumber = goldenTestCBOR fsi "test/golden/cbor/slotting/SlotNumber"
  where
    fsi = 5001 :: SlotNumber

ts_roundTripSlotNumberCBOR :: TSProperty
ts_roundTripSlotNumberCBOR = eachOfTS 1000 genSlotNumber roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- EpochSlots
--------------------------------------------------------------------------------
golden_EpochSlots :: Property
golden_EpochSlots = goldenTestCBOR sc "test/golden/cbor/slotting/EpochSlots"
  where
    sc = EpochSlots 474747

ts_roundTripEpochSlotsCBOR :: TSProperty
ts_roundTripEpochSlotsCBOR = eachOfTS 1000 genEpochSlots roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- EpochAndSlotCount
--------------------------------------------------------------------------------
golden_EpochAndSlotCount :: Property
golden_EpochAndSlotCount =
  goldenTestCBOR
    exampleEpochAndSlotCount
    "test/golden/cbor/slotting/EpochAndSlotCount"

ts_roundTripEpochAndSlotCountCBOR :: TSProperty
ts_roundTripEpochAndSlotCountCBOR = eachOfTS 1000 gen roundTripsCBORBuildable
  where
    gen = feedPMEpochSlots (\_pm es -> genEpochAndSlotCount es)

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
