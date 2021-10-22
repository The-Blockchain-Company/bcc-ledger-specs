{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Ledger.Aurum.Tools
  ( evaluateTransactionExecutionUnits,
    ScriptFailure (..),
  )
where

import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (Data, getZerepochData)
import Bcc.Ledger.Aurum.Language (Language (..))
import Bcc.Ledger.Aurum.PParams (_protocolVersion)
import Bcc.Ledger.Aurum.ZerepochScriptApi (scriptsNeeded)
import Bcc.Ledger.Aurum.Scripts
  ( CostModel (..),
    ExUnits (..),
    Script (..),
  )
import Bcc.Ledger.Aurum.Tx (DataHash, ScriptPurpose (Spending), ValidatedTx (..), rdptr)
import Bcc.Ledger.Aurum.TxBody (TxOut (..))
import Bcc.Ledger.Aurum.TxInfo (exBudgetToExUnits, txInfo, valContext)
import Bcc.Ledger.Aurum.TxWitness (RdmrPtr (..), unRedeemers, unTxDats)
import Bcc.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Slotting.EpochInfo.API (EpochInfo)
import Bcc.Slotting.Time (SystemStart)
import Data.Array (Array, (!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Records (HasField (..))
import qualified Zerepoch.V1.Ledger.Api as P
import Sophie.Spec.Ledger.Tx (TxIn)
import Sophie.Spec.Ledger.UTxO (UTxO (..), unUTxO)

-- | Failures that can be returned by 'evaluateTransactionExecutionUnits'.
data ScriptFailure c
  = -- | A redeemer was supplied that does not point to a
    --  valid zerepoch evaluation site in the given transaction.
    RedeemerNotNeeded RdmrPtr
  | -- | Missing redeemer.
    MissingScript RdmrPtr
  | -- | Missing datum.
    MissingDatum (DataHash c)
  | -- | Zerepoch evaluation error.
    ValidationFailed P.EvaluationError
  | -- | A redeemer points to a transaction input which is not
    --  present in the current UTxO.
    UnknownTxIn (TxIn c)
  | -- | A redeemer points to a transaction input which is not
    --  zerepoch locked.
    InvalidTxIn (TxIn c)
  | -- | The execution budget that was calculated by the Zerepoch
    --  evaluator is out of bounds.
    IncompatibleBudget P.ExBudget
  deriving (Show)

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnits' are intended to replace them.
evaluateTransactionExecutionUnits ::
  forall c m.
  ( CC.Crypto c,
    Monad m
  ) =>
  Core.PParams (AurumEra c) ->
  -- | The transaction.
  Core.Tx (AurumEra c) ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO (AurumEra c) ->
  -- | The epoch info, used to translate slots to POSIX time for zerepoch.
  EpochInfo m ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | The array of cost models, indexed by the supported languages.
  Array Language CostModel ->
  -- | A map from redeemer pointers to either a failure or a sufficient execution budget.
  --  The value is monadic, depending on the epoch info.
  m (Map RdmrPtr (Either (ScriptFailure c) ExUnits))
evaluateTransactionExecutionUnits pp tx utxo ei sysS costModels = do
  txinfo <- txInfo pp ei sysS utxo tx
  pure $ Map.mapWithKey (findAndCount txinfo) (unRedeemers $ getField @"txrdmrs" ws)
  where
    txb = getField @"body" tx
    ws = getField @"wits" tx
    dats = unTxDats $ getField @"txdats" ws
    scripts = getField @"txscripts" ws

    ptrToZerepochScript = Map.fromList $ do
      (sp, sh) <- scriptsNeeded utxo tx
      msb <- case Map.lookup sh scripts of
        Nothing -> pure Nothing
        Just (TimelockScript _) -> []
        Just (ZerepochScript bytes) -> pure $ Just bytes
      pointer <- case rdptr @(AurumEra c) txb sp of
        SNothing -> []
        -- Since scriptsNeeded used the transaction to create script purposes,
        -- it would be a logic error if rdptr was not able to find sp.
        SJust p -> pure p
      pure (pointer, (sp, msb))

    (CostModel costModel) = costModels ! ZerepochV1

    findAndCount ::
      P.TxInfo ->
      RdmrPtr ->
      (Data (AurumEra c), ExUnits) ->
      Either (ScriptFailure c) ExUnits
    findAndCount inf pointer (rdmr, _) = do
      (sp, mscript) <- note (RedeemerNotNeeded pointer) $ Map.lookup pointer ptrToZerepochScript
      script <- note (MissingScript pointer) mscript
      args <- case sp of
        (Spending txin) -> do
          txOut <- note (UnknownTxIn txin) $ Map.lookup txin (unUTxO utxo)
          let TxOut _ _ mdh = txOut
          dh <- note (InvalidTxIn txin) $ strictMaybeToMaybe mdh
          dat <- note (MissingDatum dh) $ Map.lookup dh dats
          pure [dat, rdmr, valContext inf sp]
        _ -> pure [rdmr, valContext inf sp]
      let pArgs = map getZerepochData args

      case snd $ P.evaluateScriptCounting P.Quiet costModel script pArgs of
        Left e -> Left $ ValidationFailed e
        Right exBudget -> note (IncompatibleBudget exBudget) $ exBudgetToExUnits exBudget
