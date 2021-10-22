{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Evie.Translation
  ( evieTranslationTests,
    evieEncodeDecodeTests,
  )
where

import Bcc.Binary
  ( ToCBOR (..),
  )
import Bcc.Ledger.Evie.Translation ()
import Bcc.Ledger.Era (TranslateEra (..))
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as MA
import qualified Sophie.Spec.Ledger.API as S
import Test.Bcc.Ledger.EraBuffet
  ( EvieEra,
    SophieEra,
    StandardCrypto,
  )
import Test.Bcc.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompatToCBOR,
  )
-- instance EraGen SophieEra
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Test.Sophie.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

type Evie = EvieEra StandardCrypto

type Sophie = SophieEra StandardCrypto

evieEncodeDecodeTests :: TestTree
evieEncodeDecodeTests =
  testGroup
    "encoded sophie types can be decoded as evie types"
    [ testProperty
        "decoding auxiliary data"
        (decodeTestAnn @(S.Metadata Evie) ([] :: [MA.AuxiliaryData Evie]))
    ]

evieTranslationTests :: TestTree
evieTranslationTests =
  testGroup
    "Evie translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (test @S.Tx),
      testProperty "SophieGenesis compatibility" (test @S.SophieGenesis),
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
    ToCBOR (f Sophie),
    TranslateEra Evie f,
    Show (TranslationError Evie f)
  ) =>
  f Sophie ->
  Bool
test = translationCompatToCBOR ([] :: [Evie]) ()
