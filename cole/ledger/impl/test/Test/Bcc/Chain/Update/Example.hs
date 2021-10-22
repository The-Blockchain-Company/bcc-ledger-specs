{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Chain.Update.Example
  ( exampleApplicationName,
    exampleProtocolVersion,
    exampleProtocolParameters,
    exampleProtocolParametersUpdate,
    exampleSoftwareVersion,
    exampleSystemTag,
    exampleInstallerHash,
    examplePayload,
    exampleProof,
    exampleProposal,
    exampleProposalBody,
    exampleUpId,
    exampleVote,
  )
where

import Bcc.Binary (Raw (..))
import Bcc.Chain.Common
  ( TxFeePolicy (..),
    TxSizeLinear (..),
    mkKnownEntropic,
    rationalToEntropicPortion,
  )
import Bcc.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import Bcc.Chain.Update
  ( ApplicationName (..),
    InstallerHash (..),
    Payload,
    Proof,
    Proposal,
    ProposalBody (..),
    ProtocolParameters (..),
    ProtocolParametersUpdate (..),
    ProtocolVersion (..),
    SoftforkRule (..),
    SoftwareVersion (..),
    SystemTag (..),
    UpId,
    Vote,
    mkProof,
    payload,
    signProposal,
    signVote,
  )
import Bcc.Crypto (ProtocolMagicId (..), serializeCborHash)
import Bcc.Prelude
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import Test.Bcc.Crypto.CBOR (getBytes)
import Test.Bcc.Crypto.Example (exampleSafeSigner)
import Test.Bcc.Prelude

exampleApplicationName :: ApplicationName
exampleApplicationName = ApplicationName "Golden"

exampleProtocolVersion :: ProtocolVersion
exampleProtocolVersion = ProtocolVersion 1 1

exampleProtocolParameters :: ProtocolParameters
exampleProtocolParameters =
  ProtocolParameters
    (999 :: Word16)
    (999 :: Natural)
    (999 :: Natural)
    (999 :: Natural)
    (999 :: Natural)
    (999 :: Natural)
    (rationalToEntropicPortion 99e-15)
    (rationalToEntropicPortion 99e-15)
    (rationalToEntropicPortion 99e-15)
    (rationalToEntropicPortion 99e-15)
    (SlotNumber 99)
    sfrule
    (TxFeePolicyTxSizeLinear tslin)
    (EpochNumber 99)
  where
    tslin = TxSizeLinear c1' c2'
    c1' = mkKnownEntropic @999
    c2' = 77 :: Rational
    sfrule =
      SoftforkRule
        (rationalToEntropicPortion 99e-15)
        (rationalToEntropicPortion 99e-15)
        (rationalToEntropicPortion 99e-15)

exampleProtocolParametersUpdate :: ProtocolParametersUpdate
exampleProtocolParametersUpdate =
  ProtocolParametersUpdate
    (Just (999 :: Word16))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just $ rationalToEntropicPortion 99e-15)
    (Just $ rationalToEntropicPortion 99e-15)
    (Just $ rationalToEntropicPortion 99e-15)
    (Just $ rationalToEntropicPortion 99e-15)
    (Just $ SlotNumber 99)
    (Just sfrule')
    (Just $ TxFeePolicyTxSizeLinear tslin')
    (Just $ EpochNumber 99)
  where
    tslin' = TxSizeLinear co1 co2
    co1 = mkKnownEntropic @999
    co2 = 77 :: Rational
    sfrule' =
      SoftforkRule
        (rationalToEntropicPortion 99e-15)
        (rationalToEntropicPortion 99e-15)
        (rationalToEntropicPortion 99e-15)

exampleSystemTag :: SystemTag
exampleSystemTag = exampleSystemTags 0 1 !! 0

exampleSystemTags :: Int -> Int -> [SystemTag]
exampleSystemTags offset count =
  map
    (toSystemTag . (* offset))
    [0 .. count - 1]
  where
    toSystemTag start = SystemTag (getText start 16)

exampleInstallerHash :: InstallerHash
exampleInstallerHash = exampleInstallerHashes 10 2 !! 1

exampleInstallerHashes :: Int -> Int -> [InstallerHash]
exampleInstallerHashes offset count =
  map
    (toInstallerHash . (* offset))
    [0 .. count - 1]
  where
    toInstallerHash start = InstallerHash . serializeCborHash . Raw $ getBytes start 128

exampleUpId :: UpId
exampleUpId = serializeCborHash exampleProposal

examplePayload :: Payload
examplePayload = payload up uv
  where
    up = Just exampleProposal
    uv = [exampleVote]

exampleProof :: Proof
exampleProof = mkProof examplePayload

exampleProposal :: Proposal
exampleProposal = signProposal pm exampleProposalBody ss
  where
    pm = ProtocolMagicId 0
    ss = exampleSafeSigner 0

exampleProposalBody :: ProposalBody
exampleProposalBody = ProposalBody bv bvm sv hm
  where
    bv = exampleProtocolVersion
    bvm = exampleProtocolParametersUpdate
    sv = exampleSoftwareVersion
    hm =
      Map.fromList $ zip (exampleSystemTags 10 5) (exampleInstallerHashes 10 5)

exampleVote :: Vote
exampleVote = signVote pm ui ar ss
  where
    pm = ProtocolMagicId 0
    ss = exampleSafeSigner 0
    ui = exampleUpId
    ar = True

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99
