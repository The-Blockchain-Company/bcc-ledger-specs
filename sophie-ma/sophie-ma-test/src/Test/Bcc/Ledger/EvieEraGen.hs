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

module Test.Bcc.Ledger.EvieEraGen
  ( -- export EraGen instance for EvieEra and helpers shared with JenEra
    quantifyTL,
    unQuantifyTL,
    someLeaf,
    genValidityInterval,
  )
where

import Bcc.Binary (serializeEncoding', toCBOR)
import Bcc.Ledger.Evie (EvieEra)
import Bcc.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import Bcc.Ledger.Coin (Coin)
import qualified Bcc.Ledger.Core as Core (AuxiliaryData)
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (Era (Crypto))
import Bcc.Ledger.Keys (KeyHash)
import Bcc.Ledger.Sophie.Constraints
  ( UsesAuxiliary,
    UsesPParams,
    UsesValue,
  )
import Bcc.Ledger.SophieMA.Timelocks (Timelock (..))
import Bcc.Ledger.SophieMA.TxBody
  ( TxBody (..),
    ValidityInterval (ValidityInterval),
  )
import Bcc.Ledger.Val (Val (zero), (<+>))
import Bcc.Slotting.Slot (SlotNo (SlotNo))
import Control.Monad (replicateM)
import Data.Hashable (hash)
import Data.Sequence.Strict (StrictSeq (..), fromList)
import qualified Data.Set as Set
import Sophie.Spec.Ledger.API (KeyRole (Witness))
import Sophie.Spec.Ledger.PParams (PParams, PParams' (..), Update)
import Sophie.Spec.Ledger.Tx (pattern Tx, pattern WitnessSet)
import Sophie.Spec.Ledger.TxBody (DCert, TxIn, TxOut (..), Wdrl)
import Test.Bcc.Ledger.SophieMA.Serialisation.Generators ()
import Test.QuickCheck (Gen, arbitrary, frequency)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Sophie.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Sophie.Spec.Ledger.Generator.Core (GenEnv (..), genCoin)
import Test.Sophie.Spec.Ledger.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Sophie.Spec.Ledger.Generator.ScriptClass
  ( Quantifier (..),
    ScriptClass (..),
  )
import Test.Sophie.Spec.Ledger.Generator.Update (genPParams, genSophiePParamsDelta)

-- ==========================================================

{------------------------------------------------------------------------------
 EraGen instance for EvieEra - This instance makes it possible to run the
 Sophie property tests for (EvieEra crypto)

 This instance is layered on top of the SophieMA instances
 in Bcc.Ledger.SophieMA.Scripts:

 `type instance Core.Script (EvieEra c) = Timelock (EvieEra c)`
 `instance ValidateScript (SophieMAEra ma c) where ...`
------------------------------------------------------------------------------}

instance (CryptoClass.Crypto c) => ScriptClass (EvieEra c) where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf @(EvieEra c)
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

instance (CryptoClass.Crypto c, Mock c) => EraGen (EvieEra c) where
  genGenesisValue (GenEnv _keySpace _scriptspace Constants {minGenesisOutputVal, maxGenesisOutputVal}) =
    genCoin minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge _utxo = genTxBody
  genEraAuxiliaryData = genAuxiliaryData
  updateEraTxBody _utxo _pp _wits (TxBody existingins outs cert wdrl _txfee vi upd ad forge) fee ins out =
    TxBody (existingins <> ins) (outs :|> out) cert wdrl fee vi upd ad forge
  genEraPParamsDelta = genSophiePParamsDelta
  genEraPParams = genPParams
  genEraWitnesses _scriptinfo setWitVKey mapScriptWit = WitnessSet setWitVKey mapScriptWit mempty
  constructTx = Tx

genTxBody ::
  forall era.
  ( UsesValue era,
    UsesAuxiliary era,
    UsesPParams era,
    EraGen era
  ) =>
  PParams era ->
  SlotNo ->
  Set.Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Gen (TxBody era, [Timelock (Crypto era)])
genTxBody _pparams slot ins outs cert wdrl fee upd ad = do
  validityInterval <- genValidityInterval slot
  let mint = zero -- the mint field is always empty for an Evie TxBody
  pure
    ( TxBody
        ins
        outs
        cert
        wdrl
        fee
        validityInterval
        upd
        ad
        mint,
      [] -- Evie does not need any additional script witnesses
    )

instance Mock c => MinGenTxout (EvieEra c) where
  calcEraMinUTxO _txout pp = _minUTxOValue pp
  addValToTxOut v (TxOut a u) = TxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    let makeTxOut (addr, val) = TxOut addr val
    pure (makeTxOut <$> zip addrs values)

{------------------------------------------------------------------------------
  SophieMA helpers, shared by Evie and Jen
------------------------------------------------------------------------------}

quantifyTL :: CryptoClass.Crypto crypto => Timelock crypto -> Quantifier (Timelock crypto)
quantifyTL (RequireAllOf xs) = AllOf (foldr (:) [] xs)
quantifyTL (RequireAnyOf xs) = AnyOf (foldr (:) [] xs)
quantifyTL (RequireMOf n xs) = MOf n (foldr (:) [] xs)
quantifyTL t = Leaf t

unQuantifyTL :: CryptoClass.Crypto crypto => Quantifier (Timelock crypto) -> Timelock crypto
unQuantifyTL (AllOf xs) = RequireAllOf (fromList xs)
unQuantifyTL (AnyOf xs) = RequireAnyOf (fromList xs)
unQuantifyTL (MOf n xs) = RequireMOf n (fromList xs)
unQuantifyTL (Leaf t) = t

genAuxiliaryData ::
  Mock crypto =>
  Constants ->
  Gen (StrictMaybe (Core.AuxiliaryData (EvieEra crypto)))
genAuxiliaryData Constants {frequencyTxWithMetadata} =
  frequency
    [ (frequencyTxWithMetadata, SJust <$> arbitrary),
      (100 - frequencyTxWithMetadata, pure SNothing)
    ]

-- | Generates a trivial validity interval that is valid for the current slot.
--
-- Note: the validity interval must be a subset of all timelock
-- script intervals that apply to the transaction. This depends on
-- which generated scripts are actually required to validate the transaction
-- (which is itself not always deterministic, e.g. 'RequireMOf n scripts').
--
-- A more sophisticated generator would compute which set of scripts
-- would validate the transaction, and from that compute a minimal
-- ValidityInterval that fits into all timelock slot ranges.
genValidityInterval :: SlotNo -> Gen ValidityInterval
genValidityInterval cs@(SlotNo currentSlot) =
  pure $
    ValidityInterval
      (SJust cs)
      (SJust . SlotNo $ currentSlot + 1)

-- | Generate some Leaf Timelock (i.e. a Signature or TimeStart or TimeExpire).
--
-- Because we don't know how these "leaf scripts" will be situated in larger scripts
-- (e.g. the script generated here might form part of a 'RequireAll' or 'RequireMOf' script)
-- we must make sure that all timelocks generated here are valid for all slots.
--
-- To achieve this we arrange the timelock scripts like so:
--  RequireAnyOf [
--     RequireAllOf [RequireTimeExpire k, RequireSignature x],
--     RequireAllOf [RequireTimeStart k, RequireSignature x]
--  ]
-- where k is arbitrary. This means that regardless of slot, there will be a
-- valid sub-branch of script.
someLeaf ::
  forall era.
  Era era =>
  KeyHash 'Witness (Crypto era) ->
  Timelock (Crypto era)
someLeaf x =
  let n = mod (hash (serializeEncoding' (toCBOR x))) 200
   in partition @era [n] [RequireSignature x]

partition ::
  forall era.
  Era era =>
  [Int] ->
  [Timelock (Crypto era)] ->
  Timelock (Crypto era)
partition splits scripts =
  RequireAnyOf . fromList $
    zipWith pair (intervals @era splits) (cycle scripts)
  where
    pair a b = RequireAllOf $ fromList [a, b]

intervals ::
  forall era.
  Era era =>
  [Int] ->
  [Timelock (Crypto era)]
intervals xs = zipWith mkInterval padded (tail padded)
  where
    padded = Nothing : (Just . SlotNo . fromIntegral <$> xs) ++ [Nothing]
    start Nothing = []
    start (Just x) = [RequireTimeStart x]
    end Nothing = []
    end (Just x) = [RequireTimeExpire x]
    mkInterval s e = RequireAllOf . fromList $ start s ++ end e
