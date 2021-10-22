{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- Embed instances for (AurumEra TestCrypto)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Bcc.Ledger.Aurum.PropertyTests where

import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.PParams (PParams' (..))
import Bcc.Ledger.Aurum.ZerepochScriptApi (collectTwoPhaseScriptInputs, evalScripts)
import Bcc.Ledger.Aurum.Rules.Bbody (AurumBBODY)
import Bcc.Ledger.Aurum.Rules.Ledger (AurumLEDGER)
import Bcc.Ledger.Aurum.Scripts (ExUnits (..), Script (..))
import Bcc.Ledger.Aurum.Tx (IsValid (..), ValidatedTx (..), totExUnits)
import Bcc.Ledger.Aurum.TxInfo (ScriptResult (..))
import Bcc.Ledger.Era (ValidateScript (..))
import Bcc.Ledger.Slot (EpochSize (..))
import Bcc.Slotting.EpochInfo (fixedEpochInfo)
import Bcc.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition
import Control.State.Transition.Trace (SourceSignalTarget (..), sourceSignalTargets)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import GHC.Records (HasField (..))
import Sophie.Spec.Ledger.LedgerState hiding (circulation)
import Sophie.Spec.Ledger.STS.Chain (CHAIN, ChainEvent (..), ChainPredicateFailure (..), ChainState (..))
import Sophie.Spec.Ledger.UTxO (UTxO (..))
import Test.Bcc.Ledger.Aurum.AurumEraGen (sumCollateral)
import Test.Bcc.Ledger.EraBuffet (TestCrypto)
import Test.QuickCheck (Property, conjoin, counterexample, (.&&.))
import Test.Sophie.Spec.Ledger.Rules.TestChain (forAllChainTrace, ledgerTraceFromBlock)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as TQC

type A = AurumEra TestCrypto

instance Embed (AurumBBODY A) (CHAIN A) where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

traceLen :: Word64
traceLen = 100

data HasZerepoch = HasZerepoch | NoZerepoch
  deriving (Show)

aurumSpecificProps ::
  SourceSignalTarget (CHAIN A) ->
  Property
aurumSpecificProps SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map aurumSpecificPropsLEDGER $
      sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock chainSt block
    pp = (esPp . nesEs . chainNes) tickedChainSt
    aurumSpecificPropsLEDGER :: SourceSignalTarget (AurumLEDGER A) -> Property
    aurumSpecificPropsLEDGER
      SourceSignalTarget
        { source = (UTxOState {_utxo = UTxO u, _deposited = dp, _fees = f}, ds),
          signal = tx,
          target = (UTxOState {_utxo = UTxO u', _deposited = dp', _fees = f'}, ds')
        } =
        let isValid' = getField @"isValid" tx
            noNewUTxO = u' `Map.isSubmapOf` u
            collateralInFees = f <> sumCollateral tx (UTxO u) == f'
            utxoConsumed = not $ u `Map.isSubmapOf` u'
            allScripts = getField @"txscripts" $ getField @"wits" tx
            hasZerepoch = if all (isNativeScript @A) allScripts then NoZerepoch else HasZerepoch
            totEU = totExUnits tx
            nonTrivialExU = exUnitsMem totEU > 0 && exUnitsSteps totEU > 0
            collected =
              -- Note that none of our zerepoch scripts use validity intervals,
              -- so it is safe to use anything for the epech info and the system start.
              case collectTwoPhaseScriptInputs
                (fixedEpochInfo (EpochSize 100) (mkSlotLength 1)) -- arbitrary
                (SystemStart $ posixSecondsToUTCTime 0) -- arbitrary
                pp
                tx
                (UTxO u) of
                Left e -> error $ "Zerepoch script collection error: " <> show e
                Right c -> c
            collectedScripts = Set.fromList $ map (\(s, _, _, _) -> s) collected
            suppliedPScrpts = Set.fromList [ZerepochScript s | ZerepochScript s <- Map.elems allScripts]
            expectedPScripts = collectedScripts == suppliedPScrpts
            allZerepochTrue = case evalScripts tx collected of
              Fails _ -> False
              Passes -> True
         in counterexample
              ( mconcat
                  [ "\nHas zerepoch scripts: ",
                    show hasZerepoch,
                    "\nIs valid: ",
                    show isValid',
                    "\nAt least one UTxO is consumed: ",
                    show utxoConsumed,
                    "\nNon trivial execution units: ",
                    show nonTrivialExU,
                    "\nReceived the expected zerepoch scripts: ",
                    show expectedPScripts,
                    "\nZerepoch scripts all evaluate to true: ",
                    show allZerepochTrue,
                    "\nNo new UTxO: ",
                    show noNewUTxO,
                    "\nThe collateral amount was added to the fees: ",
                    show collateralInFees,
                    "\nThe deposit pot is unchanged: ",
                    show (dp == dp'),
                    "\nThe delegation state is unchanged: ",
                    show (ds == ds')
                  ]
              )
              ( counterexample "At least one UTxO is consumed" utxoConsumed
                  .&&. ( case (hasZerepoch, isValid') of
                           (NoZerepoch, IsValid True) -> totEU === ExUnits 0 0
                           (NoZerepoch, IsValid False) -> counterexample "No Zerepoch scripts, but isValid == False" False
                           (HasZerepoch, IsValid True) ->
                             conjoin
                               [ counterexample "Non trivial execution units" nonTrivialExU,
                                 counterexample "Received the expected zerepoch scripts" expectedPScripts,
                                 counterexample "Zerepoch scripts all evaluate to true" allZerepochTrue
                               ]
                           (HasZerepoch, IsValid False) ->
                             conjoin
                               [ counterexample "No new UTxO" noNewUTxO,
                                 counterexample "The collateral amount was added to the fees" collateralInFees,
                                 dp === dp',
                                 ds === ds',
                                 counterexample "No failing Zerepoch scripts" $ not allZerepochTrue
                               ]
                       )
              )

aurumTraceTests :: Property
aurumTraceTests =
  forAllChainTrace @A traceLen $ \tr ->
    conjoin $ map aurumSpecificProps (sourceSignalTargets tr)

propertyTests :: TestTree
propertyTests =
  TQC.testProperty "aurum specific" aurumTraceTests
