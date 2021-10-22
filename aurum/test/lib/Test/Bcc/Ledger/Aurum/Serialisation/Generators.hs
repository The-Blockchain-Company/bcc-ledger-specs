{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Bcc.Ledger.Aurum.Serialisation.Generators where

import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (AuxiliaryData (..), Data (..))
import Bcc.Ledger.Aurum.Language
import Bcc.Ledger.Aurum.PParams
import Bcc.Ledger.Aurum.Rules.Utxo (UtxoPredicateFailure (..))
import Bcc.Ledger.Aurum.Rules.Utxos (TagMismatchDescription (..), UtxosPredicateFailure (..))
import Bcc.Ledger.Aurum.Rules.Utxow (AurumPredFail (..))
import Bcc.Ledger.Aurum.Scripts
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    Tag (..),
    alwaysFails,
    alwaysSucceeds,
  )
import Bcc.Ledger.Aurum.Tx
import Bcc.Ledger.Aurum.TxBody
  ( TxOut (..),
  )
import Bcc.Ledger.Aurum.TxInfo (FailureDescription (..), ScriptResult (..))
import Bcc.Ledger.Aurum.TxWitness
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Era (Crypto, Era, ValidateScript (..))
import Bcc.Ledger.Hashes (ScriptHash)
import Bcc.Ledger.Sophie.Constraints (UsesScript, UsesValue)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (pack)
import Numeric.Natural (Natural)
import Zerepoch.V1.Ledger.Api (defaultCostModelParams)
import qualified ZerepochTx as Zerepoch
import Test.Bcc.Ledger.SophieMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators ()

instance Arbitrary (Data era) where
  arbitrary = Data <$> arbitrary

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = do a <- x; b <- y; pure (a, b)

instance Arbitrary Zerepoch.Data where
  arbitrary = resize 5 (sized gendata)
    where
      gendata n
        | n > 0 =
          oneof
            [ (Zerepoch.I <$> arbitrary),
              (Zerepoch.B <$> arbitrary),
              (Zerepoch.Map <$> listOf (genPair (gendata (n `div` 2)) (gendata (n `div` 2)))),
              ( Zerepoch.Constr <$> fmap fromIntegral (arbitrary :: Gen Natural)
                  <*> listOf (gendata (n `div` 2))
              ),
              (Zerepoch.List <$> listOf (gendata (n `div` 2)))
            ]
      gendata _ = oneof [Zerepoch.I <$> arbitrary, Zerepoch.B <$> arbitrary]

instance
  ( UsesScript era,
    Ord (Core.Script era),
    Core.Script era ~ Script era,
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (AuxiliaryData era)
  where
  arbitrary = AuxiliaryData <$> arbitrary <*> arbitrary

instance Arbitrary Tag where
  arbitrary = elements [Spend, Mint, Cert, Rewrd]

instance Arbitrary RdmrPtr where
  arbitrary = RdmrPtr <$> arbitrary <*> arbitrary

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> arbitrary <*> arbitrary

instance (Era era) => Arbitrary (Redeemers era) where
  arbitrary = Redeemers <$> arbitrary

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Script era),
    Script era ~ Core.Script era,
    ValidateScript era,
    UsesScript era
  ) =>
  Arbitrary (TxWitness era)
  where
  arbitrary =
    TxWitness
      <$> arbitrary
      <*> arbitrary
      <*> genScripts
      <*> genData
      <*> arbitrary

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList ((\x -> (f x, x)) <$> xs)

genScripts ::
  forall era.
  ( Core.Script era ~ Script era,
    ValidateScript era,
    Arbitrary (Script era)
  ) =>
  Gen (Map (ScriptHash (Crypto era)) (Core.Script era))
genScripts = keyBy (hashScript @era) <$> (arbitrary :: Gen [Core.Script era])

genData :: forall era. Era era => Gen (TxDats era)
genData = TxDats <$> keyBy hashData <$> arbitrary

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Value era)
  ) =>
  Arbitrary (TxOut era)
  where
  arbitrary =
    TxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  forall c.
  (Mock c) =>
  Arbitrary (TxBody (AurumEra c))
  where
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
      <*> arbitrary
      <*> genMintValues @c
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary IsValid

instance Mock c => Arbitrary (ValidatedTx (AurumEra c)) where
  arbitrary =
    ValidatedTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (Script (AurumEra c)) where
  arbitrary =
    frequency
      [ (1, pure (alwaysSucceeds 1)),
        (1, pure (alwaysFails 1)),
        (10, TimelockScript <$> arbitrary)
      ]

-- ==========================
--

instance Arbitrary Language where
  arbitrary = elements [ZerepochV1]

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

instance Arbitrary CostModel where
  arbitrary = CostModel <$> traverse (const arbitrary) dcmp
    where
      dcmp = fromMaybe (error "Corrupt default cost model") defaultCostModelParams

instance Arbitrary (PParams era) where
  arbitrary =
    PParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (PParamsUpdate era) where
  arbitrary =
    PParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary FailureDescription where
  arbitrary =
    oneof
      [ (OnePhaseFailure . pack) <$> arbitrary,
        ZerepochFailure <$> (pack <$> arbitrary) <*> arbitrary
      ]

instance Arbitrary ScriptResult where
  arbitrary =
    oneof [pure Passes, Fails <$> arbitrary]

instance Arbitrary TagMismatchDescription where
  arbitrary =
    oneof [pure PassedUnexpectedly, FailedUnexpectedly <$> arbitrary]

instance Mock c => Arbitrary (UtxosPredicateFailure (AurumEra c)) where
  arbitrary =
    oneof
      [ ValidationTagMismatch <$> arbitrary <*> arbitrary,
        UpdateFailure <$> arbitrary
      ]

instance Mock c => Arbitrary (UtxoPredicateFailure (AurumEra c)) where
  arbitrary =
    oneof
      [ (BadInputsUTxO) <$> arbitrary,
        OutsideValidityIntervalUTxO <$> arbitrary <*> arbitrary,
        MaxTxSizeUTxO <$> arbitrary <*> arbitrary,
        pure InputSetEmptyUTxO,
        FeeTooSmallUTxO <$> arbitrary <*> arbitrary,
        (ValueNotConservedUTxO) <$> arbitrary <*> arbitrary,
        (OutputTooSmallUTxO) <$> arbitrary,
        (UtxosFailure) <$> arbitrary,
        (WrongNetwork) <$> arbitrary <*> arbitrary,
        (WrongNetworkWithdrawal) <$> arbitrary <*> arbitrary,
        (OutputBootAddrAttrsTooBig) <$> arbitrary,
        pure TriesToForgeBCC,
        (OutputTooBigUTxO) <$> arbitrary,
        InsufficientCollateral <$> arbitrary <*> arbitrary,
        ScriptsNotPaidUTxO <$> arbitrary,
        ExUnitsTooBigUTxO <$> arbitrary <*> arbitrary,
        CollateralContainsNonBCC <$> arbitrary
      ]

instance Mock c => Arbitrary (AurumPredFail (AurumEra c)) where
  arbitrary =
    oneof
      [ WrappedSophieEraFailure <$> arbitrary,
        MissingRedeemers <$> arbitrary,
        MissingRequiredDatums <$> arbitrary <*> arbitrary,
        PPViewHashesDontMatch <$> arbitrary <*> arbitrary
      ]

instance Mock c => Arbitrary (ScriptPurpose c) where
  arbitrary =
    oneof
      [ Minting <$> arbitrary,
        Spending <$> arbitrary,
        Rewarding <$> arbitrary,
        Certifying <$> arbitrary
      ]

instance Mock c => Arbitrary (ScriptIntegrity (AurumEra c)) where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> genData
      <*> (Set.singleton <$> (getLanguageView <$> arbitrary <*> arbitrary))
