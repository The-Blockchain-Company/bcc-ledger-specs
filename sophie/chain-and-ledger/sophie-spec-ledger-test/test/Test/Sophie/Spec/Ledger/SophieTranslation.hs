module Test.Sophie.Spec.Ledger.SophieTranslation (testGroupSophieTranslation) where

import Bcc.Ledger.Sophie (SophieEra)
import Sophie.Spec.Ledger.LedgerState (EpochState, returnRedeemAddrsToReserves)
import Sophie.Spec.Ledger.STS.Chain (totalBccES)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Sophie.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

testGroupSophieTranslation :: TestTree
testGroupSophieTranslation =
  testGroup
    "Translation from Sophie to Evie"
    [ testProperty "returning redeemers preserves bcc" propRemoveRedeemPreservesBcc
    ]

propRemoveRedeemPreservesBcc ::
  EpochState (SophieEra C_Crypto) -> Property
propRemoveRedeemPreservesBcc es =
  totalBccES es === (totalBccES . returnRedeemAddrsToReserves) es
