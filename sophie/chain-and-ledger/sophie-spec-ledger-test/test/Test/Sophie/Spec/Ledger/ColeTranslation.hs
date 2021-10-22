{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sophie.Spec.Ledger.ColeTranslation (testGroupColeTranslation) where

import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Chain.UTxO as Cole
import Bcc.Ledger.Address
import Bcc.Ledger.Coin
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Sophie (SophieEra)
import Sophie.Spec.Ledger.API.ColeTranslation
import Sophie.Spec.Ledger.TxBody
import Test.Bcc.Chain.UTxO.Gen (genCompactTxOut)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Tasty
import Test.Tasty.QuickCheck

{------------------------------------------------------------------------------
  Top-level tests
------------------------------------------------------------------------------}

testGroupColeTranslation :: TestTree
testGroupColeTranslation =
  testGroup
    "Translation from Cole to Sophie"
    [ testProperty "translateTxOut correctness" prop_translateTxOut_correctness
    ]

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

prop_translateTxOut_correctness :: Cole.CompactTxOut -> Property
prop_translateTxOut_correctness compactTxOut =
  translateTxOutColeToSophie
    @C_Crypto
    (Cole.fromCompactTxOut compactTxOut)
    === translateCompactTxOutColeToSophie compactTxOut

{------------------------------------------------------------------------------
  Reference implementation
------------------------------------------------------------------------------}

translateTxOutColeToSophie ::
  forall crypto.
  CryptoClass.Crypto crypto =>
  Cole.TxOut ->
  TxOut (SophieEra crypto)
translateTxOutColeToSophie (Cole.TxOut addr amount) =
  TxOut (translateAddr addr) (translateAmount amount)
  where
    translateAmount :: Cole.Entropic -> Coin
    translateAmount = Coin . Cole.entropicToInteger

    translateAddr :: Cole.Address -> Addr crypto
    translateAddr = AddrBootstrap . BootstrapAddress

{------------------------------------------------------------------------------
  Generators
------------------------------------------------------------------------------}

instance Arbitrary Cole.CompactTxOut where
  arbitrary = hedgehog genCompactTxOut
