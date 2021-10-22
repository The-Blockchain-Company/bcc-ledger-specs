{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Aurum.Serialisation.Tripping where

import Bcc.Binary
import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (AuxiliaryData, Data (..))
import Bcc.Ledger.Aurum.Language (Language (..))
import Bcc.Ledger.Aurum.PParams (PParams, PParamsUpdate)
import Bcc.Ledger.Aurum.Rules.Utxo (UtxoPredicateFailure)
import Bcc.Ledger.Aurum.Rules.Utxos (UtxosPredicateFailure)
import Bcc.Ledger.Aurum.Rules.Utxow (AurumPredFail)
import Bcc.Ledger.Aurum.Scripts (Script, decodeCostModel)
import Bcc.Ledger.Aurum.TxBody (TxBody)
import Bcc.Ledger.Aurum.TxWitness
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Sophie.Spec.Ledger.BlockChain (Block)
import Sophie.Spec.Ledger.Metadata (Metadata)
import qualified Sophie.Spec.Ledger.Tx as LTX
import Test.Bcc.Ledger.Aurum.Serialisation.Generators ()
import Test.Bcc.Ledger.SophieMA.Serialisation.Coders (roundTrip, roundTrip', roundTripAnn)
import Test.Bcc.Ledger.SophieMA.Serialisation.Generators ()
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes
import Test.Tasty
import Test.Tasty.QuickCheck

trippingF ::
  (Eq src, Show src, Show target, ToCBOR src) =>
  (src -> Either target (BSL.ByteString, src)) ->
  src ->
  Property
trippingF f x =
  case f x of
    Right (remaining, y)
      | BSL.null remaining ->
        x === y
    Right (remaining, _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
    Left stuff ->
      counterexample
        ( concat
            [ "Failed to decode: ",
              show stuff,
              "\nbytes: ",
              show (Base16.encode (serialize x))
            ]
        )
        False

trippingAnn ::
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR (Annotator t)
  ) =>
  t ->
  Property
trippingAnn x = trippingF roundTripAnn x

tripping :: (Eq src, Show src, ToCBOR src, FromCBOR src) => src -> Property
tripping x = trippingF roundTrip x

tests :: TestTree
tests =
  testGroup
    "Aurum CBOR round-trip"
    [ testProperty "aurum/Script" $
        trippingAnn @(Script (AurumEra C_Crypto)),
      testProperty "aurum/Data" $
        trippingAnn @(Data (AurumEra C_Crypto)),
      testProperty "aurum/Metadata" $
        trippingAnn @(Metadata (AurumEra C_Crypto)),
      testProperty "aurum/TxWitness" $
        trippingAnn @(TxWitness (AurumEra C_Crypto)),
      testProperty "aurum/TxBody" $
        trippingAnn @(TxBody (AurumEra C_Crypto)),
      testProperty "aurum/CostModel" $
        trippingF (roundTrip' toCBOR (decodeCostModel ZerepochV1)),
      testProperty "aurum/PParams" $
        tripping @(PParams (AurumEra C_Crypto)),
      testProperty "aurum/PParamsUpdate" $
        tripping @(PParamsUpdate (AurumEra C_Crypto)),
      testProperty "aurum/AuxiliaryData" $
        trippingAnn @(AuxiliaryData (AurumEra C_Crypto)),
      testProperty "aurum/AurumPredFail" $
        tripping @(AurumPredFail (AurumEra C_Crypto)),
      testProperty "aurum/UtxoPredicateFailure" $
        tripping @(UtxoPredicateFailure (AurumEra C_Crypto)),
      testProperty "aurum/UtxosPredicateFailure" $
        tripping @(UtxosPredicateFailure (AurumEra C_Crypto)),
      testProperty "Script" $
        trippingAnn @(Script (AurumEra C_Crypto)),
      testProperty "aurum/Tx" $
        trippingAnn @(LTX.Tx (AurumEra C_Crypto)),
      testProperty "aurum/Block" $
        trippingAnn @(Block (AurumEra C_Crypto))
    ]
