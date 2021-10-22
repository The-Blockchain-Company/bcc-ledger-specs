{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Aurum.Serialisation.CDDL
  ( tests,
  )
where

import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (Data)
import Bcc.Ledger.Aurum.PParams (PParamsUpdate)
import Bcc.Ledger.Aurum.Tx (ValidatedTx)
import Bcc.Ledger.Aurum.TxBody (TxOut)
import Bcc.Ledger.Aurum.TxWitness (Redeemers, TxWitness)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.SophieMA.Timelocks as MA
import qualified Data.ByteString.Lazy as BSL
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Sophie.Spec.Ledger.Serialisation.CDDLUtils
  ( cddlTest,
    cddlTest',
  )
import Test.Tasty (TestTree, testGroup, withResource)

type A = AurumEra C_Crypto

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Core.Value A) n "coin",
      cddlTest' @(Core.TxBody A) n "transaction_body",
      cddlTest' @(Core.AuxiliaryData A) n "auxiliary_data",
      cddlTest' @(MA.Timelock C_Crypto) n "native_script",
      cddlTest' @(Data A) n "zerepoch_data",
      cddlTest @(TxOut A) n "transaction_output",
      cddlTest' @(TxWitness A) n "transaction_witness_set",
      cddlTest @(PParamsUpdate A) n "protocol_param_update",
      cddlTest' @(Redeemers A) n "[* redeemer]",
      cddlTest' @(ValidatedTx A) n "transaction"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/aurum.cddl"
  crypto <- BSL.readFile "cddl-files/mock/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras
