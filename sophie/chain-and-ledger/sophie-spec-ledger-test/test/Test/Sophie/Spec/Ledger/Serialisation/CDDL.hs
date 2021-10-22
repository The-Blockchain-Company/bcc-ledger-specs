{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Sophie.Spec.Ledger.Serialisation.CDDL
  ( tests,
  )
where

import Bcc.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Bcc.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import Bcc.Crypto.KES.Sum
import Bcc.Crypto.VRF.Optimum (OptimumVRF)
import Bcc.Ledger.Address
  ( Addr,
    RewardAcnt,
  )
import Bcc.Ledger.Crypto (Crypto (..))
import Bcc.Ledger.Keys (KeyRole (Staking))
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Protocol.TOptimum.BHeader (BHBody, BHeader)
import qualified Data.ByteString.Lazy as BSL
import Sophie.Spec.Ledger.API
  ( Credential,
    DCert,
    MultiSig,
    OCert,
    ProposedPPUpdates,
    Tx,
    Update,
  )
import Sophie.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Sophie.Spec.Ledger.BlockChain (LaxBlock)
import Sophie.Spec.Ledger.Metadata (Metadata)
import Sophie.Spec.Ledger.PParams (PParamsUpdate)
import Sophie.Spec.Ledger.TxBody
  ( StakePoolRelay,
    TxBody,
    TxIn,
    TxOut,
  )
import Test.Sophie.Spec.Ledger.Serialisation.CDDLUtils
  ( cddlGroupTest,
    cddlTest,
    cddlTest',
  )
import Test.Tasty (TestTree, testGroup, withResource)

-- Crypto family as used in production Sophie
-- TODO: we really need a central location for all the Crypto and Era families.
-- I think we need something like Test.Bcc.Ledger.EraBuffet
-- (currently in the sophie-ma-test package)
-- that lives outside any era specific package.
data SophieC

instance Bcc.Ledger.Crypto.Crypto SophieC where
  type DSIGN SophieC = Ed25519DSIGN
  type KES SophieC = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF SophieC = OptimumVRF
  type HASH SophieC = Blake2b_256
  type ADDRHASH SophieC = Blake2b_224

type SophieE = SophieEra SophieC

tests :: Int -> TestTree
tests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest' @(BHeader SophieC) n "header",
      cddlTest' @(BootstrapWitness SophieC) n "bootstrap_witness",
      cddlTest @(BHBody SophieC) n "header_body",
      cddlGroupTest @(OCert SophieC) n "operational_cert",
      cddlTest @(Addr SophieC) n "address",
      cddlTest @(RewardAcnt SophieC) n "reward_account",
      cddlTest @(Credential 'Staking SophieC) n "stake_credential",
      cddlTest' @(TxBody SophieE) n "transaction_body",
      cddlTest @(TxOut SophieE) n "transaction_output",
      cddlTest @StakePoolRelay n "relay",
      cddlTest @(DCert SophieC) n "certificate",
      cddlTest @(TxIn SophieC) n "transaction_input",
      cddlTest' @(Metadata SophieE) n "transaction_metadata",
      cddlTest' @(MultiSig SophieC) n "multisig_script",
      cddlTest @(Update SophieE) n "update",
      cddlTest @(ProposedPPUpdates SophieE) n "proposed_protocol_parameter_updates",
      cddlTest @(PParamsUpdate SophieE) n "protocol_param_update",
      cddlTest' @(Tx SophieE) n "transaction",
      cddlTest' @(LaxBlock SophieE) n "block"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/sophie.cddl"
  crypto <- BSL.readFile "cddl-files/real/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  --extras contains the types whose restrictions cannot be expressed in CDDL
  pure $ base <> crypto <> extras
