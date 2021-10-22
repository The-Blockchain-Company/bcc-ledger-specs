{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bcc.Ledger.Aurum.Translation where

import Bcc.Binary
  ( DecoderError,
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    serialize,
  )
import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Genesis (AurumGenesis (..), extendPPWithGenesis)
import Bcc.Ledger.Aurum.PParams (PParams, PParamsUpdate, extendPP)
import Bcc.Ledger.Aurum.Tx (IsValid (..), ValidatedTx (..))
import Bcc.Ledger.Aurum.TxBody (TxOut (..))
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Crypto (Crypto)
import Bcc.Ledger.Era
  ( PreviousEra,
    TranslateEra (..),
    TranslationContext,
    translateEra',
  )
import Bcc.Ledger.Jen (JenEra)
import Control.Monad.Except (Except, throwError)
import Data.Coders
import Data.Text (Text)
import Sophie.Spec.Ledger.API
  ( EpochState (..),
    NewEpochState (..),
    SophieGenesis,
    StrictMaybe (..),
  )
import qualified Sophie.Spec.Ledger.API as API
import qualified Sophie.Spec.Ledger.PParams as Sophie
import qualified Sophie.Spec.Ledger.Tx as LTX
import qualified Sophie.Spec.Ledger.TxBody as Sophie

--------------------------------------------------------------------------------
-- Translation from Jen to Aurum
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

type instance PreviousEra (AurumEra c) = JenEra c

type instance TranslationContext (AurumEra c) = AurumGenesis

instance
  (Crypto c) =>
  TranslateEra (AurumEra c) NewEpochState
  where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes
        }

instance Crypto c => TranslateEra (AurumEra c) SophieGenesis where
  translateEra ctxt genesis =
    return
      API.SophieGenesis
        { API.sgSystemStart = API.sgSystemStart genesis,
          API.sgNetworkMagic = API.sgNetworkMagic genesis,
          API.sgNetworkId = API.sgNetworkId genesis,
          API.sgActiveSlotsCoeff = API.sgActiveSlotsCoeff genesis,
          API.sgSecurityParam = API.sgSecurityParam genesis,
          API.sgEpochLength = API.sgEpochLength genesis,
          API.sgSlotsPerKESPeriod = API.sgSlotsPerKESPeriod genesis,
          API.sgVestMultiple = API.sgVestMultiple genesis,
          API.sgVestedDelegs = API.sgVestedDelegs genesis,
          API.sgMaxKESEvolutions = API.sgMaxKESEvolutions genesis,
          API.sgSlotLength = API.sgSlotLength genesis,
          API.sgUpdateQuorum = API.sgUpdateQuorum genesis,
          API.sgMaxEntropicSupply = API.sgMaxEntropicSupply genesis,
          API.sgProtocolParams = translateEra' ctxt (API.sgProtocolParams genesis),
          API.sgGenDelegs = API.sgGenDelegs genesis,
          API.sgInitialFunds = API.sgInitialFunds genesis,
          API.sgStaking = API.sgStaking genesis
        }

newtype Tx era = Tx {unTx :: Core.Tx era}

instance
  ( Crypto c,
    Core.Tx (AurumEra c) ~ ValidatedTx (AurumEra c)
  ) =>
  TranslateEra (AurumEra c) Tx
  where
  type TranslationError (AurumEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    bdy <- translateViaCBORAnn "txbody" $ LTX.body tx
    txwits <- translateViaCBORAnn "txwitness" $ LTX.wits tx
    -- transactions from Jen era always pass script ("phase 2") validation
    aux <- case LTX.auxiliaryData tx of
      SNothing -> pure SNothing
      SJust axd -> SJust <$> translateViaCBORAnn "auxiliarydata" axd
    let validating = IsValid True
    pure $ Tx $ ValidatedTx bdy txwits validating aux

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

translateViaCBORAnn :: (ToCBOR a, FromCBOR (Annotator b)) => Text -> a -> Except DecoderError b
translateViaCBORAnn name x =
  case decodeAnnotator name fromCBOR (serialize x) of
    Right newx -> pure newx
    Left decoderError -> throwError decoderError

instance (Crypto c, Functor f) => TranslateEra (AurumEra c) (API.PParams' f)

instance Crypto c => TranslateEra (AurumEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translatePParams ctxt $ esPrevPp es,
          esPp = translatePParams ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (AurumEra c) API.LedgerState where
  translateEra ctxt ls =
    return
      API.LedgerState
        { API._utxoState = translateEra' ctxt $ API._utxoState ls,
          API._delegationState = API._delegationState ls
        }

instance Crypto c => TranslateEra (AurumEra c) API.UTxOState where
  translateEra ctxt us =
    return
      API.UTxOState
        { API._utxo = translateEra' ctxt $ API._utxo us,
          API._deposited = API._deposited us,
          API._fees = API._fees us,
          API._ppups = translateEra' ctxt $ API._ppups us
        }

instance Crypto c => TranslateEra (AurumEra c) API.UTxO where
  translateEra _ctxt utxo =
    return $ API.UTxO $ translateTxOut <$> API.unUTxO utxo

instance Crypto c => TranslateEra (AurumEra c) API.PPUPState where
  translateEra ctxt ps =
    return
      API.PPUPState
        { API.proposals = translateEra' ctxt $ API.proposals ps,
          API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        }

instance Crypto c => TranslateEra (AurumEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    return $ API.ProposedPPUpdates $ fmap translatePParamsUpdate ppup

translateTxOut ::
  Core.TxOut (JenEra c) -> Core.TxOut (AurumEra c)
translateTxOut (Sophie.TxOutCompact addr value) = TxOutCompact addr value

-- extendPP with type: extendPP :: Sophie.PParams' f era1 -> ... -> PParams' f era2
-- Is general enough to work for both
-- (PParams era)       = (PParams' Identity era)    and
-- (PParamsUpdate era) = (PParams' StrictMaybe era)

translatePParams ::
  AurumGenesis -> Sophie.PParams (JenEra c) -> PParams (AurumEra c)
translatePParams = flip extendPPWithGenesis

translatePParamsUpdate ::
  Sophie.PParamsUpdate (JenEra c) -> PParamsUpdate (AurumEra c)
translatePParamsUpdate pp =
  extendPP pp SNothing SNothing SNothing SNothing SNothing SNothing SNothing SNothing
