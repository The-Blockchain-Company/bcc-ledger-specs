{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Jen.Translation
  ( jenTranslationTests,
    jenEncodeDecodeTests,
  )
where

import Bcc.Binary
  ( ToCBOR (..),
  )
import Bcc.Ledger.Era (TranslateEra (..))
import Bcc.Ledger.Jen.Translation ()
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as MA
import qualified Sophie.Spec.Ledger.API as S
import Test.Bcc.Ledger.EvieEraGen ()
-- import Evie EraGen instance
import Test.Bcc.Ledger.EraBuffet
  ( EvieEra,
    JenEra,
    StandardCrypto,
  )
import Test.Bcc.Ledger.SophieMA.Serialisation.Generators ()
import Test.Bcc.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompatToCBOR,
  )
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Sophie.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

type Evie = EvieEra StandardCrypto

type Jen = JenEra StandardCrypto

jenEncodeDecodeTests :: TestTree
jenEncodeDecodeTests =
  testGroup
    "encoded evie types can be decoded as jen types"
    [ testProperty
        "decoding metadata"
        (decodeTestAnn @(S.Metadata Evie) ([] :: [MA.AuxiliaryData Jen]))
    ]

jenTranslationTests :: TestTree
jenTranslationTests =
  testGroup
    "Jen translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (test @S.Tx),
      testProperty "ProposedPPUpdates compatibility" (test @S.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @S.PPUPState),
      testProperty "TxOut compatibility" (test @S.TxOut),
      testProperty "UTxO compatibility" (test @S.UTxO),
      testProperty "UTxOState compatibility" (test @S.UTxOState),
      testProperty "LedgerState compatibility" (test @S.LedgerState),
      testProperty "EpochState compatibility" (test @S.EpochState),
      testProperty "WitnessSet compatibility" (test @S.WitnessSet),
      testProperty "Update compatibility" (test @S.Update)
    ]

test ::
  forall f.
  ( ToCBOR (f Evie),
    ToCBOR (f Jen),
    TranslateEra Jen f,
    Show (TranslationError Jen f)
  ) =>
  f Evie ->
  Bool
test = translationCompatToCBOR ([] :: [Jen]) ()
