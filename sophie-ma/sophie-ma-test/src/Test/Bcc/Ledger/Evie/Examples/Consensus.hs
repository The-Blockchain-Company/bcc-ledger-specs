{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Bcc.Ledger.Evie.Examples.Consensus where

import Bcc.Ledger.Evie (EvieEra)
import Bcc.Ledger.AuxiliaryData
import Bcc.Ledger.Coin
import Bcc.Ledger.Core
import Bcc.Ledger.Crypto
import Bcc.Ledger.Era
import Bcc.Ledger.SophieMA
import Bcc.Ledger.SophieMA.AuxiliaryData
import Bcc.Ledger.SophieMA.Timelocks
import Bcc.Ledger.SophieMA.TxBody
import Bcc.Slotting.Slot
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import Sophie.Spec.Ledger.API
import Test.Sophie.Spec.Ledger.Examples.Consensus
import Test.Sophie.Spec.Ledger.Orphans ()
import Test.Sophie.Spec.Ledger.SentryUtils hiding (mkVRFKeyPair)

type StandardEvie = EvieEra StandardCrypto

-- | SophieLedgerExamples for Evie era
ledgerExamplesEvie :: SophieLedgerExamples StandardEvie
ledgerExamplesEvie =
  defaultSophieLedgerExamples
    (mkWitnessesPreAurum (Proxy @StandardEvie))
    id
    exampleCoin
    exampleTxBodyEvie
    exampleAuxiliaryDataMA

exampleTxBodyEvie :: Bcc.Ledger.SophieMA.TxBody.TxBody StandardEvie
exampleTxBodyEvie = exampleTxBodyMA exampleCoin

exampleTxBodyMA ::
  forall era.
  ( SophieBasedEra' era,
    PParamsDelta era ~ PParams' StrictMaybe era
  ) =>
  Bcc.Ledger.Core.Value era ->
  Bcc.Ledger.SophieMA.TxBody.TxBody era
exampleTxBodyMA value =
  Bcc.Ledger.SophieMA.TxBody.TxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ TxOut (mkAddr (examplePayKey, exampleStakeKey)) value
        ]
    )
    exampleCerts
    exampleWithdrawals
    (Coin 3)
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4)))
    (SJust (Update exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
    value
  where
    -- Dummy hash to decouple from the auxiliary data in 'exampleTx'.
    auxiliaryDataHash :: AuxiliaryDataHash (Bcc.Ledger.Era.Crypto era)
    auxiliaryDataHash =
      AuxiliaryDataHash $ mkDummySafeHash (Proxy @(Bcc.Ledger.Era.Crypto era)) 30

exampleAuxiliaryDataMA :: Bcc.Ledger.Crypto.Crypto c => Bcc.Ledger.SophieMA.AuxiliaryData.AuxiliaryData (SophieMAEra ma c)
exampleAuxiliaryDataMA =
  AuxiliaryData
    exampleMetadataMap
    (StrictSeq.fromList [exampleScriptMA])

exampleScriptMA :: Bcc.Ledger.Crypto.Crypto c => Script (SophieMAEra ma c)
exampleScriptMA =
  Bcc.Ledger.SophieMA.Timelocks.RequireMOf 2 $
    StrictSeq.fromList
      [ Bcc.Ledger.SophieMA.Timelocks.RequireAllOf $
          StrictSeq.fromList
            [ RequireTimeStart (SlotNo 0),
              RequireTimeExpire (SlotNo 9)
            ],
        Bcc.Ledger.SophieMA.Timelocks.RequireAnyOf $
          StrictSeq.fromList
            [ Bcc.Ledger.SophieMA.Timelocks.RequireSignature (mkKeyHash 0),
              Bcc.Ledger.SophieMA.Timelocks.RequireSignature (mkKeyHash 1)
            ],
        Bcc.Ledger.SophieMA.Timelocks.RequireSignature (mkKeyHash 100)
      ]
