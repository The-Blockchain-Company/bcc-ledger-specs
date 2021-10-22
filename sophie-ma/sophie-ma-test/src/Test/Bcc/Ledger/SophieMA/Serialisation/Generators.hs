{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Bcc.Ledger.SophieMA.Serialisation.Generators
  ( sizedTimelock,
    maxTimelockDepth,
    genMintValues,
  )
where

import Bcc.Binary (Annotator, FromCBOR, ToCBOR (toCBOR))
import Bcc.Crypto.Hash (HashAlgorithm, hashWithSerialiser)
import qualified Bcc.Crypto.Hash as Hash
import Bcc.Ledger.Evie (EvieEra)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era (Crypto, Era)
import Bcc.Ledger.Jen (JenEra)
import qualified Bcc.Ledger.Jen.Value as ConcreteValue
import qualified Bcc.Ledger.Jen.Value as Jen
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as MA
import qualified Bcc.Ledger.SophieMA.Rules.Utxo as MA.STS
import Bcc.Ledger.SophieMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Bcc.Ledger.SophieMA.Timelocks as MA (Timelock (..))
import qualified Bcc.Ledger.SophieMA.TxBody as MA (TxBody (..))
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Word (Word64)
import Generic.Random (genericArbitraryU)
import Sophie.Spec.Ledger.API hiding (SignedDSIGN, TxBody (..))
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    choose,
    genericShrink,
    listOf,
    oneof,
    resize,
    shrink,
    vectorOf,
  )
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Sophie.Spec.Ledger.Generator.Metadata (genMetadata')
import Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Sophie.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty.QuickCheck (Gen)

{-------------------------------------------------------------------------------
  SophieMAEra Generators
  Generators used for roundtrip tests, generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

mkDummyHash :: forall h a. HashAlgorithm h => Int -> Hash.Hash h a
mkDummyHash = coerce . hashWithSerialiser @h toCBOR

maxTimelockDepth :: Int
maxTimelockDepth = 3

maxTimelockListLens :: Int
maxTimelockListLens = 5

sizedTimelock ::
  CC.Crypto crypto =>
  Int ->
  Gen (Timelock crypto)
sizedTimelock 0 = MA.RequireSignature . KeyHash . mkDummyHash <$> arbitrary
sizedTimelock n =
  oneof
    [ MA.RequireSignature . KeyHash . mkDummyHash <$> arbitrary,
      MA.RequireAllOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n -1)))
            ),
      MA.RequireAnyOf
        <$> ( fromList
                <$> resize
                  maxTimelockListLens
                  (listOf (sizedTimelock (n -1)))
            ),
      do
        subs <- resize maxTimelockListLens (listOf (sizedTimelock (n -1)))
        let i = length subs
        MA.RequireMOf <$> choose (0, i) <*> pure (fromList subs),
      RequireTimeStart <$> arbitrary,
      RequireTimeExpire <$> arbitrary
    ]

-- TODO Generate metadata with script preimages
instance
  forall era c.
  ( Era era,
    c ~ Crypto era,
    Mock c,
    FromCBOR (Annotator (Core.Script era)),
    ToCBOR (Core.Script era),
    Ord (Core.Script era),
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (MA.AuxiliaryData era)
  where
  -- Why do we use the \case instead of a do statement? like this:
  --
  -- @
  -- arbitrary = do
  --   Metadata m <- genMetadata'
  --   MA.AuxiliaryData m <$> genScriptSeq
  -- @
  --
  -- The above leads to an error about a failable
  -- pattern, despite the pattern being COMPLETE, resulting
  -- in an unsatisfied `MonadFail` constraint.
  arbitrary =
    genMetadata' >>= \case
      Metadata m -> MA.AuxiliaryData m <$> (genScriptSeq @era)

genScriptSeq ::
  forall era. Arbitrary (Core.Script era) => Gen (StrictSeq (Core.Script era))
genScriptSeq = do
  n <- choose (0, 3)
  l <- vectorOf n arbitrary
  pure (fromList l)

{-------------------------------------------------------------------------------
  JenEra Generators
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (MA.TxBody (JenEra c)) where
  arbitrary =
    MA.TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues

instance Mock c => Arbitrary (Jen.PolicyID c) where
  arbitrary = Jen.PolicyID <$> arbitrary

instance Mock c => Arbitrary (Jen.Value c) where
  arbitrary = valueFromListBounded @Word64 <$> arbitrary <*> arbitrary

  shrink (Jen.Value bcc assets) =
    concat
      [ -- Shrink the BCC value
        flip Jen.Value assets <$> shrink bcc,
        -- Shrink the non-BCC assets by reducing the list length
        Jen.Value
          bcc
          <$> shrink assets
      ]

-- | When generating values for the mint field, we do two things:
--
-- - Fix the BCC value to 0
-- - Allow both positive and negative quantities
genMintValues :: forall c. Mock c => Gen (Jen.Value c)
genMintValues = valueFromListBounded @Int64 0 <$> arbitrary

-- | Variant on @valueFromList@ that makes sure that generated values stay
-- bounded within the range of a given integral type.
valueFromListBounded ::
  forall i crypto.
  (Bounded i, Integral i) =>
  i ->
  [(Jen.PolicyID crypto, Jen.AssetName, i)] ->
  Jen.Value crypto
valueFromListBounded (fromIntegral -> bcc) =
  foldr
    (\(p, n, fromIntegral -> i) ans -> ConcreteValue.insert comb p n i ans)
    (Jen.Value bcc Map.empty)
  where
    comb :: Integer -> Integer -> Integer
    comb a b =
      max
        (fromIntegral $ minBound @i)
        (min (fromIntegral $ maxBound @i) (a + b))

instance Arbitrary Jen.AssetName where
  arbitrary = Jen.AssetName . BS.pack . take 32 . BS.unpack <$> arbitrary

instance Mock c => Arbitrary (MA.STS.UtxoPredicateFailure (JenEra c)) where
  arbitrary = genericArbitraryU

{-------------------------------------------------------------------------------
  EvieEra Generators
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (MA.TxBody (EvieEra c)) where
  arbitrary =
    MA.TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure (Coin 0)

instance Mock c => Arbitrary (Timelock c) where
  arbitrary = sizedTimelock maxTimelockDepth

instance Arbitrary ValidityInterval where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Mock c => Arbitrary (MA.STS.UtxoPredicateFailure (EvieEra c)) where
  arbitrary = genericArbitraryU
