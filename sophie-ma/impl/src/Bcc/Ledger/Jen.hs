{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bcc.Ledger.Jen
  ( JenEra,
    Self,
    TxOut,
    Value,
    TxBody,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
    Tx,
  )
where

import Bcc.Ledger.Crypto (Crypto)
import qualified Bcc.Ledger.Crypto as CC
import qualified Bcc.Ledger.Era as E (Era (Crypto))
import qualified Bcc.Ledger.Jen.Value as V (Value)
import Bcc.Ledger.SophieMA
import Bcc.Ledger.SophieMA.Rules.EraMapping ()
import Bcc.Ledger.SophieMA.Rules.Utxo (consumed, scaledMinDeposit)
import Bcc.Ledger.SophieMA.Rules.Utxow ()
import Bcc.Ledger.SophieMA.Timelocks (Timelock)
import Bcc.Ledger.Val (Val ((<->)), coin, inject)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Sophie.Spec.Ledger.API hiding (TxBody)
import Sophie.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import Sophie.Spec.Ledger.LedgerState (minfee)
import qualified Sophie.Spec.Ledger.PParams as Sophie (PParamsUpdate)

instance OptimumCrypto c => ApplyTx (JenEra c)

instance OptimumCrypto c => ApplyBlock (JenEra c)

instance OptimumCrypto c => GetLedgerView (JenEra c)

instance Crypto c => CanStartFromGenesis (JenEra c) where
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
              (DPState (def {_genDelegs = GenDelegs genDelegs}{_vestedDelegs = VestedDelegs vestedDelegs}) def)
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
        coin $
          inject (word64ToCoin (sgMaxEntropicSupply sg))
            <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      vestedDelegs = sgVestedDelegs sg
      pp = sgProtocolParams sg

instance OptimumCrypto c => SophieBasedEra (JenEra c)

instance CC.Crypto c => CLI (JenEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses = addSophieKeyWitnesses

  evaluateMinEntropicOutput pp (TxOut _ v) = scaledMinDeposit v (_minUTxOValue pp)

-- Self-Describing type synomyms

type JenEra c = SophieMAEra 'Jen c

type Self c = SophieMAEra 'Jen c

type Script era = Timelock (E.Crypto era)

type Value era = V.Value (E.Crypto era)

type PParamsDelta era = Sophie.PParamsUpdate era
