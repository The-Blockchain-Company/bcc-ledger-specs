{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Bcc.Ledger.Jen.Examples.Consensus where

import Bcc.Ledger.Core
import Bcc.Ledger.Crypto
import Bcc.Ledger.Jen (JenEra)
import Bcc.Ledger.Jen.Value
import qualified Data.Map.Strict as Map (singleton)
import Data.Proxy
import Test.Bcc.Ledger.Evie.Examples.Consensus
import Test.Sophie.Spec.Ledger.Examples.Consensus
import Test.Sophie.Spec.Ledger.Orphans ()

type StandardJen = JenEra StandardCrypto

-- | SophieLedgerExamples for Evie era
ledgerExamplesJen :: SophieLedgerExamples StandardJen
ledgerExamplesJen =
  defaultSophieLedgerExamples
    (mkWitnessesPreAurum (Proxy @StandardJen))
    id
    (exampleMultiAssetValue 1)
    exampleTxBodyJen
    exampleAuxiliaryDataMA

exampleMultiAssetValue ::
  forall c.
  Bcc.Ledger.Crypto.Crypto c =>
  Int ->
  Bcc.Ledger.Jen.Value.Value c
exampleMultiAssetValue x =
  Value 100 $ Map.singleton policyId $ Map.singleton couttsCoin 1000
  where
    policyId :: PolicyID c
    policyId = PolicyID $ mkScriptHash x

    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"

exampleTxBodyJen :: Bcc.Ledger.Core.TxBody StandardJen
exampleTxBodyJen = exampleTxBodyMA (exampleMultiAssetValue 1)
