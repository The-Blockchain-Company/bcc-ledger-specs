{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Sophie.Spec.Ledger.Generator.SophieEraGen (genCoin) where

import qualified Bcc.Crypto.DSIGN as DSIGN
import qualified Bcc.Crypto.KES as KES
import Bcc.Crypto.Util (SignableRepresentation)
import Bcc.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import Bcc.Ledger.Crypto (DSIGN, KES)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era (Crypto)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot (SlotNo (..))
import Bcc.Ledger.Val ((<+>))
import Control.Monad (replicateM)
import Data.Sequence.Strict (StrictSeq ((:|>)))
import Data.Set (Set)
import Sophie.Spec.Ledger.API
  ( Coin (..),
    DCert,
    OptimumCrypto,
    Update,
  )
import Sophie.Spec.Ledger.PParams (PParams, PParams' (..))
import Sophie.Spec.Ledger.STS.EraMapping ()
import Sophie.Spec.Ledger.Scripts (MultiSig (..))
import Sophie.Spec.Ledger.Tx
  ( Tx (..),
    TxIn (..),
    TxOut (..),
    pattern WitnessSet,
  )
import Sophie.Spec.Ledger.TxBody (TxBody (TxBody, _inputs, _outputs, _txfee), Wdrl (..))
import Test.QuickCheck (Gen)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (Mock)
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
import Test.Sophie.Spec.Ledger.VestedSealUtils (SophieTest)

{------------------------------------------------------------------------------
  SophieEra instances for EraGen and ScriptClass
 -----------------------------------------------------------------------------}

instance
  ( OptimumCrypto c,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation,
    KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  EraGen (SophieEra c)
  where
  genGenesisValue
    ( GenEnv
        _keySpace
        _scriptspace
        Constants {minGenesisOutputVal, maxGenesisOutputVal}
      ) =
      genCoin minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge _utxo = genTxBody
  genEraAuxiliaryData = genMetadata

  updateEraTxBody _utxo _pp _wits body' fee ins out =
    body'
      { _txfee = fee,
        _inputs = (_inputs body') <> ins,
        _outputs = (_outputs body') :|> out
      }
  genEraPParamsDelta = genSophiePParamsDelta
  genEraPParams = genPParams

  genEraWitnesses _ setWitVKey mapScriptWit = WitnessSet setWitVKey mapScriptWit mempty
  constructTx = Tx

instance CC.Crypto c => ScriptClass (SophieEra c) where
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
  SophieEra generators
 -----------------------------------------------------------------------------}

genTxBody ::
  (SophieTest era) =>
  PParams era ->
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

instance Mock c => MinGenTxout (SophieEra c) where
  calcEraMinUTxO _txout pp = (_minUTxOValue pp)
  addValToTxOut v (TxOut a u) = TxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    let makeTxOut (addr, val) = TxOut addr val
    pure (makeTxOut <$> zip addrs values)
