{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sophie.Spec.Ledger.API.Genesis where

import Bcc.Ledger.Core (EraRule)
import Bcc.Ledger.Crypto (Crypto)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Val (Val ((<->)))
import Control.State.Transition (STS (State))
import Data.Default.Class (Default, def)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Sophie.Spec.Ledger.API.Types
  ( AccountState (AccountState),
    Coin (Coin),
    DPState (DPState),
    DState (_genDelegs),
    DState (_vestedDelegs),
    EpochState (EpochState),
    GenDelegs (GenDelegs),
    VestedDelegs (VestedDelegs),
    LedgerState (LedgerState),
    NewEpochState (NewEpochState),
    PoolDistr (PoolDistr),
    SophieGenesis (sgGenDelegs, sgMaxEntropicSupply, sgProtocolParams, sgVestedDelegs),
    StrictMaybe (SNothing),
    UTxOState (UTxOState),
    balance,
    genesisUTxO,
    word64ToCoin,
  )
import Sophie.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)

-- | Indicates that this era may be bootstrapped from 'SophieGenesis'.
class CanStartFromGenesis era where
  -- | Additional genesis configuration necessary for this era.
  type AdditionalGenesisConfig era :: Type

  type AdditionalGenesisConfig era = ()

  -- | Construct an initial state given a 'SophieGenesis' and any appropriate
  -- 'AdditionalGenesisConfig' for the era.
  initialState ::
    SophieGenesis era ->
    AdditionalGenesisConfig era ->
    NewEpochState era

instance
  ( Crypto c,
    Default (State (EraRule "PPUP" (SophieEra c)))
  ) =>
  CanStartFromGenesis (SophieEra c)
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
              (DPState (def {_genDelegs = GenDelegs genDelegs} 
                            {_vestedDelegs = VestedDelegs vestedDelegs}) def)
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
      vestedDelegs = sgVestedDelegs sg
      pp = sgProtocolParams sg
