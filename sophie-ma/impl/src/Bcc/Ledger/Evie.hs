{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bcc.Ledger.Evie
  ( EvieEra,
    Self,
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
    Tx,
    Witnesses,
  )
where

import Bcc.Ledger.Crypto (Crypto)
import qualified Bcc.Ledger.Crypto as CC
import qualified Bcc.Ledger.Era as E (Era (Crypto))
import Bcc.Ledger.SophieMA
import Bcc.Ledger.SophieMA.Rules.EraMapping ()
import Bcc.Ledger.SophieMA.Rules.Utxo (consumed)
import Bcc.Ledger.SophieMA.Timelocks (Timelock)
import Bcc.Ledger.SophieMA.TxBody ()
import Bcc.Ledger.Val (Val ((<->)))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Sophie.Spec.Ledger.API hiding (PParams, Tx, TxBody, TxOut, WitnessSet)
import Sophie.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import Sophie.Spec.Ledger.LedgerState (minfee)
import qualified Sophie.Spec.Ledger.PParams as Sophie (PParamsUpdate)
import Sophie.Spec.Ledger.Tx (WitnessSet)

type EvieEra = SophieMAEra 'Evie

instance OptimumCrypto c => ApplyTx (EvieEra c)

instance OptimumCrypto c => ApplyBlock (EvieEra c)

instance OptimumCrypto c => GetLedgerView (EvieEra c)

instance
  ( Crypto c
  ) =>
  CanStartFromGenesis (EvieEra c)
  where
  initialState sg () =
    NewEpochState
      initialEpochNo
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      ( EpochState
          (AccountState (Coin 0) reserves)
          emptySnapShots
          ( LedgerState
              ( UTxOState
                  initialUtxo
                  (Coin 0)
                  (Coin 0)
                  def
              )
              (DPState (def {_genDelegs = GenDelegs genDelegs}) def)
          )
          pp
          pp
          def
      )
      SNothing
      (PoolDistr Map.empty)
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        word64ToCoin (sgMaxEntropicSupply sg)
          <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

instance OptimumCrypto c => SophieBasedEra (EvieEra c)

instance CC.Crypto c => CLI (EvieEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses = addSophieKeyWitnesses

  evaluateMinEntropicOutput pp _out = _minUTxOValue pp

-- Self-Describing type synomyms

type Self c = SophieMAEra 'Evie c

type Script era = Timelock (E.Crypto era)

type Value era = Coin

type Witnesses era = WitnessSet (E.Crypto era)

type PParamsDelta era = Sophie.PParamsUpdate era
