{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Bcc.Ledger.Aurum.Translation
  ( tests,
  )
where

import Bcc.Binary
  ( ToCBOR (..),
  )
import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (AuxiliaryData)
import Bcc.Ledger.Aurum.Genesis (AurumGenesis (..))
import Bcc.Ledger.Aurum.Translation (Tx (..))
import Bcc.Ledger.Aurum.Tx (toCBORForSizeComputation)
import Bcc.Ledger.Aurum.TxBody (TxBody)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Era (TranslateEra (..))
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as MA
import qualified Bcc.Ledger.SophieMA.TxBody as MA
import Data.Typeable (Typeable)
import qualified Sophie.Spec.Ledger.API as API
import Test.Bcc.Ledger.EvieEraGen ()
import Test.Bcc.Ledger.EraBuffet
  ( JenEra,
    StandardCrypto,
  )
import Test.Bcc.Ledger.SophieMA.Serialisation.Generators ()
import Test.Bcc.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompat,
    translationCompatToCBOR,
  )
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Sophie.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

type Jen = JenEra StandardCrypto

type Aurum = AurumEra StandardCrypto

tests :: TestTree
tests =
  testGroup
    "Translation"
    [ aurumTranslationTests,
      aurumEncodeDecodeTests
    ]

aurumEncodeDecodeTests :: TestTree
aurumEncodeDecodeTests =
  testGroup
    "encoded jen types can be decoded as aurum types"
    [ testProperty
        "decoding auxilliary"
        (decodeTestAnn @(MA.AuxiliaryData Jen) ([] :: [AuxiliaryData Aurum])),
      testProperty
        "decoding txbody"
        (decodeTestAnn @(MA.TxBody Jen) ([] :: [TxBody Aurum])),
      testProperty
        "decoding witnesses"
        (decodeTestAnn @(Core.Witnesses Jen) ([] :: [Core.Witnesses Aurum]))
    ]

aurumTranslationTests :: TestTree
aurumTranslationTests =
  testGroup
    "Aurum translation binary compatibiliby tests"
    [ testProperty "Core.Tx compatibility" testTx,
      testProperty "ProposedPPUpdates compatibility" (test @API.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @API.PPUPState),
      testProperty "UTxO compatibility" (test @API.UTxO),
      testProperty "UTxOState compatibility" (test @API.UTxOState),
      testProperty "LedgerState compatibility" (test @API.LedgerState)
    ]

deriving newtype instance
  (Arbitrary (Core.Tx era)) =>
  Arbitrary (Tx era)

deriving newtype instance
  (Typeable era, ToCBOR (Core.Tx era)) =>
  ToCBOR (Tx era)

deriving newtype instance
  (Show (Core.Tx era)) =>
  Show (Tx era)

dummyAurumGenesis :: AurumGenesis
dummyAurumGenesis = undefined

test ::
  forall f.
  ( ToCBOR (f Jen),
    ToCBOR (f Aurum),
    TranslateEra Aurum f,
    Show (TranslationError Aurum f)
  ) =>
  f Jen ->
  Bool
test = translationCompatToCBOR ([] :: [Aurum]) dummyAurumGenesis

testTx :: Tx Jen -> Bool
testTx =
  translationCompat @Aurum
    dummyAurumGenesis
    (toCBORForSizeComputation . unTx)
    toCBOR
