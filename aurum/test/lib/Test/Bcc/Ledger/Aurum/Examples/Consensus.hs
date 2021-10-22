{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Aurum.Examples.Consensus where

import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (AuxiliaryData (..), AuxiliaryDataHash (..), Data (..), hashData)
import Bcc.Ledger.Aurum.PParams (PParams' (..), emptyPParams, emptyPParamsUpdate)
import Bcc.Ledger.Aurum.Scripts (ExUnits (..), Script (..), alwaysFails, alwaysSucceeds)
import qualified Bcc.Ledger.Aurum.Scripts as Tag (Tag (..))
import Bcc.Ledger.Aurum.Tx (IsValid (..), ValidatedTx (..))
import Bcc.Ledger.Aurum.TxBody (TxBody (..), TxOut (..))
import Bcc.Ledger.Aurum.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..), TxWitness (..))
import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import Bcc.Ledger.Coin (Coin (..))
import Bcc.Ledger.Core (TxBody)
import Bcc.Ledger.Crypto (StandardCrypto)
import Bcc.Ledger.Era (ValidateScript (hashScript))
import Bcc.Ledger.Keys (asWitness)
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Ledger.SophieMA.Timelocks (Timelock (..), ValidityInterval (..))
import Bcc.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified ZerepochTx as Zerepoch
import Sophie.Spec.Ledger.API
  ( ApplyTxError (..),
    Credential (..),
    Network (..),
    NewEpochState (..),
    ProposedPPUpdates (..),
    RewardAcnt (..),
    TxId (..),
    TxIn (..),
    Update (..),
    Wdrl (..),
  )
import Sophie.Spec.Ledger.STS.Delegs (DelegsPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Sophie.Spec.Ledger.Tx (Tx (..))
import Sophie.Spec.Ledger.UTxO (makeWitnessesVKey)
import qualified Test.Bcc.Ledger.Jen.Examples.Consensus as SLE
import qualified Test.Sophie.Spec.Ledger.Examples.Consensus as SLE
import Test.Sophie.Spec.Ledger.Orphans ()
import Test.Sophie.Spec.Ledger.SentryUtils (mkAddr)

type StandardAurum = AurumEra StandardCrypto

-- | SophieLedgerExamples for Aurum era
ledgerExamplesAurum :: SLE.SophieLedgerExamples StandardAurum
ledgerExamplesAurum =
  SLE.SophieLedgerExamples
    { SLE.sleBlock = SLE.exampleSophieLedgerBlock exampleTransactionInBlock,
      SLE.sleHashHeader = SLE.exampleHashHeader (Proxy @StandardAurum),
      SLE.sleTx = exampleTransactionInBlock,
      SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @StandardAurum (SLE.mkKeyHash 1),
      SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100),
            Right (ScriptHashObj (SLE.mkScriptHash 1)),
            Right (KeyHashObj (SLE.mkKeyHash 2))
          ],
      SLE.sleResultExamples = resultExamples,
      SLE.sleNewEpochState = exampleAurumNewEpochState,
      SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1
    }
  where
    resultExamples =
      SLE.SophieResultExamples
        { SLE.srePParams = def,
          SLE.sreProposedPPUpdates = examplePPPU,
          SLE.srePoolDistr = SLE.examplePoolDistr,
          SLE.sreNonMyopicRewards = SLE.exampleNonMyopicRewards,
          SLE.sreSophieGenesis = SLE.testSophieGenesis
        }
    examplePPPU =
      ProposedPPUpdates $
        Map.singleton
          (SLE.mkKeyHash 0)
          (emptyPParamsUpdate {_collateralPercentage = SJust 150})

exampleTxBodyAurum :: Bcc.Ledger.Core.TxBody StandardAurum
exampleTxBodyAurum =
  TxBody
    (Set.fromList [TxIn (TxId (SLE.mkDummySafeHash Proxy 1)) 0]) --inputs
    (Set.fromList [TxIn (TxId (SLE.mkDummySafeHash Proxy 2)) 1]) --collateral
    ( StrictSeq.fromList
        [ TxOut
            (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
            (SLE.exampleMultiAssetValue 2)
            (SJust $ SLE.mkDummySafeHash Proxy 1) --outputs
        ]
    )
    SLE.exampleCerts --txcerts
    ( Wdrl $
        Map.singleton
          (RewardAcnt Testnet (SLE.keyToCredential SLE.exampleStakeKey))
          (Coin 100) --txwdrls
    )
    (Coin 999) --txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) --txvldt
    ( SJust $
        Update
          ( ProposedPPUpdates $
              Map.singleton
                (SLE.mkKeyHash 1)
                (emptyPParamsUpdate {_maxBHSize = SJust 4000})
          )
          (EpochNo 0)
    ) --txUpdates
    (Set.singleton $ SLE.mkKeyHash 212) --reqSignerHashes
    (SLE.exampleMultiAssetValue 3) --mint
    (SJust $ SLE.mkDummySafeHash (Proxy @StandardCrypto) 42) --scriptIntegrityHash
    (SJust . AuxiliaryDataHash $ SLE.mkDummySafeHash (Proxy @StandardCrypto) 42) --adHash
    (SJust Mainnet) --txnetworkid

datumExample :: Data StandardAurum
datumExample = Data (Zerepoch.I 191)

redeemerExample :: Data StandardAurum
redeemerExample = Data (Zerepoch.I 919)

exampleTx :: Tx StandardAurum
exampleTx =
  Tx
    exampleTxBodyAurum
    ( TxWitness
        (makeWitnessesVKey (hashAnnotated exampleTxBodyAurum) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        (Map.singleton (hashScript @StandardAurum $ alwaysSucceeds 3) (alwaysSucceeds 3)) -- txscripts
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (RdmrPtr Tag.Spend 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        AuxiliaryData
          SLE.exampleMetadataMap -- metadata
          (StrictSeq.fromList [alwaysFails 2, TimelockScript $ RequireAllOf mempty]) -- Scripts
    )

exampleTransactionInBlock :: ValidatedTx StandardAurum
exampleTransactionInBlock = ValidatedTx b w (IsValid True) a
  where
    (Tx b w a) = exampleTx

exampleAurumNewEpochState :: NewEpochState StandardAurum
exampleAurumNewEpochState =
  SLE.exampleNewEpochState
    (SLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams {_coinsPerUTxOWord = Coin 1})
