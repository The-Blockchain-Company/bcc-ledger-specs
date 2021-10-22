{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sophie.Spec.Ledger.Serialisation.Generators () where

import Bcc.Ledger.Sophie (SophieEra)
import Generic.Random (genericArbitraryU)
import Sophie.Spec.Ledger.API (TxBody (TxBody))
import Sophie.Spec.Ledger.PParams (PParams)
import qualified Sophie.Spec.Ledger.STS.Utxo as STS
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    shrink,
  )
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators ()

{-------------------------------------------------------------------------------
  SophieEra Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance Mock c => Arbitrary (TxBody (SophieEra c)) where
  arbitrary =
    TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (STS.UtxoPredicateFailure (SophieEra c)) where
  arbitrary = genericArbitraryU
  shrink _ = []

-- | Note that this instance is a little off - it is an era-independent
-- generator for something which is only valid in certain eras. Its sole use is
-- for `SophieGenesis`, a somewhat confusing type which is in fact used as the
-- genesis for multiple eras.
instance Arbitrary (PParams era) where
  arbitrary = genericArbitraryU
