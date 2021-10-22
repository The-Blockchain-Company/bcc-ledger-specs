{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Bcc.Ledger.Example
  ( -- export EraGen instance for ExampleEra and helpers shared with JenEra
    genCoin,
  )
where

import qualified Bcc.Crypto.DSIGN as DSIGN
import qualified Bcc.Crypto.KES as KES
import Bcc.Crypto.Util (SignableRepresentation)
import Bcc.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Crypto (DSIGN, KES)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era (Crypto)
import Bcc.Ledger.Example (ExampleEra)
import Bcc.Ledger.Slot (SlotNo (..))
import Bcc.Ledger.Val ((<+>))
import Control.Monad (replicateM)
import Data.Sequence.Strict (StrictSeq ((:|>)))
import Data.Set (Set)
import Generic.Random (genericArbitraryU)
import Sophie.Spec.Ledger.API
  ( Coin (..),
    DCert,
    OptimumCrypto,
    Update,
  )
import Sophie.Spec.Ledger.PParams (PParams' (..))
import Sophie.Spec.Ledger.STS.EraMapping ()
import qualified Sophie.Spec.Ledger.STS.Utxo as STS
import Sophie.Spec.Ledger.Scripts (MultiSig (..))
import Sophie.Spec.Ledger.Tx
  ( TxIn (..),
    TxOut (..),
    WitnessSetHKD (WitnessSet),
    pattern Tx,
  )
import Sophie.Spec.Ledger.TxBody (TxBody (TxBody, _inputs, _outputs, _txfee), Wdrl (..))
import Test.QuickCheck
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes
import Test.Sophie.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Sophie.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    genCoin,
    genNatural,
  )
import Test.Sophie.Spec.Ledger.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Sophie.Spec.Ledger.Generator.Metadata (genMetadata)
import Test.Sophie.Spec.Ledger.Generator.ScriptClass
  ( Quantifier (..),
    ScriptClass (..),
  )
import Test.Sophie.Spec.Ledger.Generator.Trace.Chain ()
import Test.Sophie.Spec.Ledger.Generator.Update (genPParams, genSophiePParamsDelta)
import Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Sophie.Spec.Ledger.VestedSealUtils (SophieTest)

{------------------------------------------------------------------------------
  ExampleEra instances for EraGen and ScriptClass
 -----------------------------------------------------------------------------}

instance
  ( OptimumCrypto c,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation,
    KES.Signable (KES c) ~ SignableRepresentation,
    MinGenTxout (ExampleEra c)
  ) =>
  EraGen (ExampleEra c)
  where
  genGenesisValue
    ( GenEnv
        _keySpace
        _ScriptSpace
        Constants {minGenesisOutputVal, maxGenesisOutputVal}
      ) =
      genCoin minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge _utxo = genTxBody
  genEraAuxiliaryData = genMetadata
  genEraPParamsDelta = genSophiePParamsDelta
  genEraPParams = genPParams
  genEraWitnesses _triple setWitVKey mapScriptWit = WitnessSet setWitVKey mapScriptWit mempty

  updateEraTxBody _utxo _pp _wits body fee ins out =
    body
      { _txfee = fee,
        _inputs = (_inputs body) <> ins,
        _outputs = (_outputs body) :|> out
      }
  constructTx = Tx

instance CC.Crypto c => ScriptClass (ExampleEra c) where
  basescript _proxy = RequireSignature
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  quantify _ (RequireAllOf xs) = AllOf xs
  quantify _ (RequireAnyOf xs) = AnyOf xs
  quantify _ (RequireMOf n xs) = MOf n xs
  quantify _ t = Leaf t
  unQuantify _ (AllOf xs) = RequireAllOf xs
  unQuantify _ (AnyOf xs) = RequireAnyOf xs
  unQuantify _ (MOf n xs) = RequireMOf n xs
  unQuantify _ (Leaf t) = t

{------------------------------------------------------------------------------
  ExampleEra generators
 -----------------------------------------------------------------------------}

genTxBody ::
  (SophieTest era) =>
  Core.PParams era ->
  SlotNo ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Gen (TxBody era, [MultiSig (Crypto era)])
genTxBody _pparams slot inputs outputs certs wdrls fee update adHash = do
  ttl <- genTimeToLive slot
  return
    ( TxBody
        inputs
        outputs
        certs
        wdrls
        fee
        ttl
        update
        adHash,
      [] -- Sophie does not need any additional script witnesses
    )

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

instance Mock c => Arbitrary (TxBody (ExampleEra c)) where
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

instance Mock c => Arbitrary (STS.UtxoPredicateFailure (ExampleEra c)) where
  arbitrary = genericArbitraryU
  shrink _ = []

instance Mock c => MinGenTxout (ExampleEra c) where
  calcEraMinUTxO _txout pp = (_minUTxOValue pp)
  addValToTxOut v (TxOut a u) = TxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    let makeTxOut (addr, val) = TxOut addr val
    pure (makeTxOut <$> zip addrs values)
