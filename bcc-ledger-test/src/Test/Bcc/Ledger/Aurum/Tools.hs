{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Aurum.Tools (tests, testExUnitCalculation) where

import Bcc.Ledger.Aurum.Language (Language (..))
import Bcc.Ledger.Aurum.PParams (PParams, PParams' (..), ProtVer (..))
import Bcc.Ledger.Aurum.Rules.Utxos (UTXOS)
import Bcc.Ledger.Aurum.Scripts (CostModel, ExUnits (..))
import Bcc.Ledger.Aurum.Tools (evaluateTransactionExecutionUnits)
import Bcc.Ledger.Aurum.Tx
  ( ValidatedTx (..),
  )
import Bcc.Ledger.Aurum.TxInfo (exBudgetToExUnits, transExUnits)
import Bcc.Ledger.Aurum.TxWitness (RdmrPtr, Redeemers (..), txrdmrs)
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Keys (GenDelegs (..))
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Bcc.Slotting.Slot (EpochSize (..), SlotNo (..))
import Bcc.Slotting.Time (SystemStart, mkSlotLength)
import Control.State.Transition.Extended (TRC (..))
import Data.Array (Array, array)
import Data.Default.Class (Default (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Sophie.Spec.Ledger.LedgerState (UTxOState (..))
import Sophie.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Sophie.Spec.Ledger.UTxO (UTxO, makeWitnessVKey)
import Test.Bcc.Ledger.Aurum.ZerepochScripts (defaultCostModel)
import Test.Bcc.Ledger.Examples.TwoPhaseValidation (A, datumExample1, initUTxO, someKeys, testSystemStart, validatingBody, validatingRedeemersEx1)
import Test.Bcc.Ledger.Generic.Proof (Evidence (Mock), Proof (Aurum))
import Test.Bcc.Ledger.Generic.Updaters
import Test.Sophie.Spec.Ledger.SentryUtils (applySTSTest, runSophieBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.QuickCheck (Gen, Property, chooseBoundedIntegral, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup "ExUnit tools" $
    [ testProperty "Zerepoch ExUnit translation round-trip" exUnitsTranslationRoundTrip,
      testCase "calculate ExUnits" exampleExUnitCalc
    ]

genExUnits :: Gen ExUnits
genExUnits = ExUnits <$> genUnit <*> genUnit
  where
    genUnit :: Gen Word64
    genUnit = chooseBoundedIntegral (0, 2 ^ (63 :: Word64) - 1)

-- ExUnits should remain intact when translating to and from the zerepoch type
exUnitsTranslationRoundTrip :: Gen Property
exUnitsTranslationRoundTrip = do
  e <- genExUnits
  let result = (exBudgetToExUnits . transExUnits) e
  pure $
    counterexample
      ( "Before: " <> show (Just e)
          <> "\n After: "
          <> show result
      )
      $ result == Just e

-- checks zerepoch script validation against a tx which has had
-- its ex units replaced by the output of evaluateTransactionExecutionUnits
testExUnitCalculation ::
  MonadFail m =>
  Core.Tx A ->
  UTxOState A ->
  UtxoEnv A ->
  EpochInfo m ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m ()
testExUnitCalculation tx utxoState ue ei ss costmdls err = do
  tx' <- updateTxExUnits tx utxo ei ss costmdls err
  _ <-
    failLeft err $
      runSophieBase $
        applySTSTest @(UTXOS A) (TRC (ue, utxoState, tx'))
  pure ()
  where
    utxo = _utxo utxoState

exampleExUnitCalc :: IO ()
exampleExUnitCalc =
  testExUnitCalculation
    exampleTx
    ustate
    uenv
    exampleEpochInfo
    testSystemStart
    costmodels
    assertFailure

exampleTx :: Core.Tx A
exampleTx =
  let pf = Aurum Mock
   in newTx
        Override
        pf
        [ Body (validatingBody pf),
          Witnesses'
            [ AddrWits [makeWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)],
              ScriptWits [always 3 pf],
              DataWits [datumExample1],
              RdmrWits validatingRedeemersEx1
            ]
        ]

exampleEpochInfo :: Monad m => EpochInfo m
exampleEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

uenv :: UtxoEnv A
uenv = UtxoEnv (SlotNo 0) pparams mempty (GenDelegs mempty)

costmodels :: Array Language CostModel
costmodels = array (ZerepochV1, ZerepochV1) [(ZerepochV1, fromJust defaultCostModel)]

ustate :: UTxOState A
ustate =
  UTxOState
    { _utxo = initUTxO (Aurum Mock),
      _deposited = Coin 0,
      _fees = Coin 0,
      _ppups = def
    }

-- Requires ex units, but there are no fees
pparams :: PParams A
pparams =
  newPParams
    (Aurum Mock)
    [ Costmdls $ Map.singleton ZerepochV1 $ fromJust defaultCostModel,
      MaxValSize 1000000000,
      MaxTxExUnits $ ExUnits 100000000 100000000,
      MaxBlockExUnits $ ExUnits 100000000 100000000,
      ProtocolVersion $ ProtVer 5 0 0
    ]

updateTxExUnits ::
  MonadFail m =>
  Core.Tx A ->
  UTxO A ->
  EpochInfo m ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m (Core.Tx A)
updateTxExUnits tx utxo ei ss costmdls err = do
  -- rdmrs :: Map RdmrPtr ExUnits
  rdmrs <-
    traverse (failLeft err)
      =<< evaluateTransactionExecutionUnits pparams tx utxo ei ss costmdls
  pure (replaceRdmrs tx rdmrs)

replaceRdmrs :: Core.Tx A -> Map RdmrPtr ExUnits -> Core.Tx A
replaceRdmrs tx rdmrs = tx {wits = wits'}
  where
    wits' = (wits tx) {txrdmrs = newrdmrs}
    newrdmrs = foldr replaceRdmr (txrdmrs (wits tx)) (Map.assocs rdmrs)

    replaceRdmr :: (RdmrPtr, ExUnits) -> Redeemers A -> Redeemers A
    replaceRdmr (ptr, ex) x@(Redeemers r) =
      case Map.lookup ptr r of
        Just (dat, _ex) -> Redeemers $ Map.insert ptr (dat, ex) r
        Nothing -> x

failLeft :: (Monad m, Show e) => (String -> m a) -> Either e a -> m a
failLeft _ (Right a) = pure a
failLeft err (Left e) = err (show e)
