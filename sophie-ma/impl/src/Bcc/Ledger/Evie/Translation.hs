{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bcc.Ledger.Evie.Translation where

import Bcc.Binary
  ( DecoderError,
    decodeAnnotator,
    fromCBOR,
    serialize,
  )
import Bcc.Ledger.Evie (EvieEra)
import Bcc.Ledger.Crypto (Crypto)
import Bcc.Ledger.Era hiding (Crypto)
import Bcc.Ledger.Sophie (SophieEra)
import Control.Monad.Except (throwError)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Sophie.Spec.Ledger.API
import qualified Sophie.Spec.Ledger.LedgerState as LS
  ( returnRedeemAddrsToReserves,
  )
import Sophie.Spec.Ledger.Tx (decodeWits)

--------------------------------------------------------------------------------
-- Translation from Sophie to Evie
--
-- The instances below are needed by the consensus layer. Do not remove any of
-- them without coordinating with consensus.
--
-- Please add auxiliary instances and other declarations at the bottom of this
-- module, not in the list below so that it remains clear which instances the
-- consensus layer needs.
--
-- WARNING: when a translation instance currently uses the default
-- 'TranslationError', i.e., 'Void', it means the consensus layer relies on it
-- being total. Do not change it!
--------------------------------------------------------------------------------

type instance PreviousEra (EvieEra c) = SophieEra c

-- | Currently no context is needed to translate from Sophie to Evie.

-- Note: if context is needed, please coordinate with consensus, who will have
-- to provide the context in the right place.
type instance TranslationContext (EvieEra c) = ()

instance Crypto c => TranslateEra (EvieEra c) NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ LS.returnRedeemAddrsToReserves . nesEs $ nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes
        }

instance forall c. Crypto c => TranslateEra (EvieEra c) Tx where
  type TranslationError (EvieEra c) Tx = DecoderError
  translateEra _ctx tx =
    case decodeAnnotator "tx" fromCBOR (serialize tx) of
      Right newTx -> pure newTx
      Left decoderError -> throwError decoderError

instance Crypto c => TranslateEra (EvieEra c) SophieGenesis where
  translateEra ctxt genesis =
    return
      SophieGenesis
        { sgSystemStart = sgSystemStart genesis,
          sgNetworkMagic = sgNetworkMagic genesis,
          sgNetworkId = sgNetworkId genesis,
          sgActiveSlotsCoeff = sgActiveSlotsCoeff genesis,
          sgSecurityParam = sgSecurityParam genesis,
          sgEpochLength = sgEpochLength genesis,
          sgSlotsPerKESPeriod = sgSlotsPerKESPeriod genesis,
          sgVestMultiple = sgVestMultiple genesis,
          sgVestedDelegs = sgVestedDelegs genesis,
          sgMaxKESEvolutions = sgMaxKESEvolutions genesis,
          sgSlotLength = sgSlotLength genesis,
          sgUpdateQuorum = sgUpdateQuorum genesis,
          sgMaxEntropicSupply = sgMaxEntropicSupply genesis,
          sgProtocolParams = translateEra' ctxt (sgProtocolParams genesis),
          sgGenDelegs = sgGenDelegs genesis,
          sgInitialFunds = sgInitialFunds genesis,
          sgStaking = sgStaking genesis
        }

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (EvieEra c) (PParams' f) where
  translateEra _ pp =
    return $
      PParams
        { _minfeeA = _minfeeA pp,
          _minfeeB = _minfeeB pp,
          _maxBBSize = _maxBBSize pp,
          _maxTxSize = _maxTxSize pp,
          _maxBHSize = _maxBHSize pp,
          _keyDeposit = _keyDeposit pp,
          _poolDeposit = _poolDeposit pp,
          _eMax = _eMax pp,
          _nOpt = _nOpt pp,
          _a0 = _a0 pp,
          _rho = _rho pp,
          _tau = _tau pp,
          _d = _d pp,
          _extraEntropy = _extraEntropy pp,
          _protocolVersion = _protocolVersion pp,
          _minUTxOValue = _minUTxOValue pp,
          _minPoolCost = _minPoolCost pp
        }

instance Crypto c => TranslateEra (EvieEra c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (EvieEra c) PPUPState where
  translateEra ctxt ps =
    return
      PPUPState
        { proposals = translateEra' ctxt $ proposals ps,
          futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (EvieEra c) TxOut where
  translateEra () (TxOutCompact addr cfval) =
    pure $ TxOutCompact (coerce addr) cfval

instance Crypto c => TranslateEra (EvieEra c) UTxO where
  translateEra ctxt utxo =
    return $ UTxO $ Map.map (translateEra' ctxt) $ unUTxO utxo

instance Crypto c => TranslateEra (EvieEra c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { _utxo = translateEra' ctxt $ _utxo us,
          _deposited = _deposited us,
          _fees = _fees us,
          _ppups = translateEra' ctxt $ _ppups us
        }

instance Crypto c => TranslateEra (EvieEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { _utxoState = translateEra' ctxt $ _utxoState ls,
          _delegationState = _delegationState ls
        }

instance Crypto c => TranslateEra (EvieEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translateEra' ctxt $ esPrevPp es,
          esPp = translateEra' ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (EvieEra c) WitnessSet where
  type TranslationError (EvieEra c) WitnessSet = DecoderError
  translateEra _ctx ws =
    case decodeAnnotator "witnessSet" decodeWits (serialize ws) of
      Right new -> pure new
      Left decoderError -> throwError decoderError

instance Crypto c => TranslateEra (EvieEra c) Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en
