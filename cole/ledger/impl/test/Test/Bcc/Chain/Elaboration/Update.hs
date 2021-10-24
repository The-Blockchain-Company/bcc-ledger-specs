{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Chain.Elaboration.Update
  ( elaboratePParams,
    elaborateProtocolVersion,
    elaborateSoftwareVersion,
    elaborateUpdateProposal,
    elaborateVote,
  )
where

import Cole.Spec.Ledger.Core (unSlotCount)
import Cole.Spec.Ledger.Core.Omniscient (signatureData, signatureVKey)
import qualified Cole.Spec.Ledger.GlobalParams as GP
import qualified Cole.Spec.Ledger.Update as Abstract
import qualified Bcc.Chain.Common as Concrete
import qualified Bcc.Chain.Slotting as Concrete
import qualified Bcc.Chain.Update as Concrete
import qualified Bcc.Chain.Update.Proposal as Proposal
import Bcc.Crypto (ProtocolMagicId)
import qualified Bcc.Crypto.Hashing as H
import Bcc.Prelude
import Data.Coerce (coerce)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Test.Bcc.Chain.Elaboration.Keys (elaborateVKey, vKeyToSafeSigner)
import Test.Bcc.Chain.Genesis.Dummy (dummyProtocolParameters)

elaboratePParams :: Abstract.PParams -> Concrete.ProtocolParameters
elaboratePParams pps =
  Concrete.ProtocolParameters
    { Concrete.ppScriptVersion = fromIntegral $ Abstract._scriptVersion pps,
      Concrete.ppSlotDuration = Concrete.ppSlotDuration dummyProtocolParameters,
      Concrete.ppMaxBlockSize = 4096 * Abstract._maxBkSz pps,
      Concrete.ppMaxHeaderSize = 95 * Abstract._maxHdrSz pps,
      Concrete.ppMaxTxSize = 4096 * Abstract._maxTxSz pps,
      Concrete.ppMaxProposalSize = 4096 * Abstract._maxPropSz pps,
      Concrete.ppMpcThd = Concrete.rationalToEntropicPortion 0,
      Concrete.ppHeavyDelThd = Concrete.rationalToEntropicPortion 0,
      Concrete.ppUpdateVoteThd = Concrete.rationalToEntropicPortion 0,
      Concrete.ppUpdateProposalThd = Concrete.rationalToEntropicPortion 0,
      Concrete.ppUpdateProposalTTL =
        Concrete.SlotNumber $
          unSlotCount $
            Abstract._upTtl pps,
      Concrete.ppSoftforkRule =
        Concrete.SoftforkRule
          { Concrete.srInitThd = Concrete.rationalToEntropicPortion 0,
            -- See 'upAdptThd' in 'module Bcc.Chain.Update.ProtocolParameters'
            Concrete.srMinThd =
              Concrete.rationalToEntropicPortion $
                realToFrac $
                  Abstract._upAdptThd pps,
            Concrete.srThdDecrement = Concrete.rationalToEntropicPortion 0
          },
      Concrete.ppTxFeePolicy =
        elaborateFeePolicy
          (Abstract._factorA pps)
          (Abstract._factorB pps),
      Concrete.ppUnlockStakeEpoch = Concrete.EpochNumber maxBound
    }

elaborateFeePolicy ::
  Abstract.FactorA ->
  Abstract.FactorB ->
  Concrete.TxFeePolicy
elaborateFeePolicy (Abstract.FactorA a) (Abstract.FactorB b) =
  Concrete.TxFeePolicyTxSizeLinear $ Concrete.TxSizeLinear aC bC
  where
    aC = intToEntropic a
    bC = fromIntegral b % fromIntegral GP.c :: Rational

    intToEntropic :: Int -> Concrete.Entropic
    intToEntropic x =
      case Concrete.mkEntropic (fromIntegral x) of
        Left err -> panic $ "intToEntropic: " <> show err
        Right l -> l

elaborateProtocolVersion ::
  Abstract.ProtVer ->
  Concrete.ProtocolVersion
elaborateProtocolVersion (Abstract.ProtVer major sentry) =
  -- TODO: the abstract version numbers should have the same type as the
  -- concrete ones!
  Concrete.ProtocolVersion
    (fromIntegral major)
    (fromIntegral sentry)
    

elaborateSoftwareVersion ::
  Abstract.SwVer ->
  Concrete.SoftwareVersion
elaborateSoftwareVersion abstractVersion =
  Concrete.SoftwareVersion applicationName' applicationVersion'
  where
    Abstract.SwVer
      (Abstract.ApName applicationName)
      (Abstract.ApVer applicationVersion) = abstractVersion
    applicationName' = Concrete.ApplicationName $ Text.pack applicationName
    applicationVersion' = fromIntegral applicationVersion :: Concrete.NumSoftwareVersion

elaborateUpdateProposal ::
  ProtocolMagicId ->
  Abstract.UProp ->
  Concrete.AProposal ()
elaborateUpdateProposal protocolMagicId abstractProposal =
  Concrete.unsafeProposal
    body
    issuer
    proposalSignature
  where
    body = elaborateProposalBody abstractProposal
    issuer = elaborateVKey $ Abstract._upIssuer abstractProposal
    signer = signatureVKey $ Abstract._upSig abstractProposal
    signedProposalBody =
      elaborateUpSD $
        signatureData $
          Abstract._upSig abstractProposal
    -- To elaborate the signature, we extract the signer and the (abstract)
    -- data that was signed from the signature of the abstract proposal. We
    -- cannot simply sign the concrete proposal data, since the abstract signed
    -- data might differ from the data in the certificate (for instance due to
    -- invalid data generation).
    --
    proposalSignature =
      Concrete.signatureForProposal
        protocolMagicId
        signedProposalBody
        safeSigner
    safeSigner = vKeyToSafeSigner signer

elaborateProposalBody ::
  Abstract.UProp ->
  Concrete.ProposalBody
elaborateProposalBody = elaborateUpSD . Abstract.getUpSigData

elaborateUpSD :: Abstract.UpSD -> Concrete.ProposalBody
elaborateUpSD
  ( protocolVersion,
    protocolParameters,
    softwareVersion,
    systemTags,
    _metadata
    ) =
    Proposal.ProposalBody
      { Proposal.protocolVersion =
          elaborateProtocolVersion protocolVersion,
        Proposal.protocolParametersUpdate =
          justifyProtocolParameters $ elaboratePParams protocolParameters,
        Proposal.softwareVersion =
          elaborateSoftwareVersion softwareVersion,
        Proposal.metadata =
          Map.fromList $ zip concreteSystemTags concreteSystemHashes
      }
    where
      concreteSystemTags =
        fmap elaborateSystemTag $ Set.toList systemTags
      -- TODO: we might need different hashes here, which means that either the
      -- elaborators should be able to generate random data, or the abstract
      -- update payload should include (an abstract version of) these hashes.
      concreteSystemHashes =
        repeat $ Concrete.InstallerHash $ coerce $ H.serializeCborHash ("" :: ByteString)

-- | Convert a 'ProtocolParameters' value to a 'ProtocolParametersUpdate'
justifyProtocolParameters ::
  Concrete.ProtocolParameters ->
  Concrete.ProtocolParametersUpdate
justifyProtocolParameters parameters =
  Concrete.ProtocolParametersUpdate
    { Concrete.ppuScriptVersion = Just $ Concrete.ppScriptVersion parameters,
      Concrete.ppuSlotDuration = Just $ Concrete.ppSlotDuration parameters,
      Concrete.ppuMaxBlockSize = Just $ Concrete.ppMaxBlockSize parameters,
      Concrete.ppuMaxHeaderSize = Just $ Concrete.ppMaxHeaderSize parameters,
      Concrete.ppuMaxTxSize = Just $ Concrete.ppMaxTxSize parameters,
      Concrete.ppuMaxProposalSize = Just $ Concrete.ppMaxProposalSize parameters,
      Concrete.ppuMpcThd = Just $ Concrete.ppMpcThd parameters,
      Concrete.ppuHeavyDelThd = Just $ Concrete.ppHeavyDelThd parameters,
      Concrete.ppuUpdateVoteThd = Just $ Concrete.ppUpdateVoteThd parameters,
      Concrete.ppuUpdateProposalThd = Just $ Concrete.ppUpdateProposalThd parameters,
      Concrete.ppuUpdateProposalTTL = Just $ Concrete.ppUpdateProposalTTL parameters,
      Concrete.ppuSoftforkRule = Just $ Concrete.ppSoftforkRule parameters,
      Concrete.ppuTxFeePolicy = Just $ Concrete.ppTxFeePolicy parameters,
      Concrete.ppuUnlockStakeEpoch = Just $ Concrete.ppUnlockStakeEpoch parameters
    }

elaborateSystemTag :: Abstract.STag -> Concrete.SystemTag
elaborateSystemTag = Concrete.SystemTag . Text.pack

elaborateVote ::
  ProtocolMagicId ->
  Map Abstract.UpId Concrete.UpId ->
  Abstract.Vote ->
  Concrete.AVote ()
elaborateVote protocolMagicId proposalsIdMap abstractVote =
  Concrete.unsafeVote
    issuer
    (elaborateProposalId proposalsIdMap abstractProposalId)
    voteSignature
  where
    abstractProposalId = Abstract._vPropId abstractVote
    issuer = elaborateVKey $ Abstract._vCaster abstractVote
    voteSignature =
      Concrete.signatureForVote
        protocolMagicId
        signedUpId
        True -- We assume the decision to be always constant
        safeSigner
    signedUpId =
      elaborateProposalId proposalsIdMap $
        signatureData $
          Abstract._vSig abstractVote
    safeSigner =
      vKeyToSafeSigner $ signatureVKey $ Abstract._vSig abstractVote

-- | Lookup the proposal id in the map. If the proposal id is not in the map
-- then return the hash of the abstract proposal id.
--
-- The reason why we return the hash of the abstract proposal id if the
-- proposal id is not in the given map is that when producing invalid abstract
-- votes, we need to elaborate a non-existing abstract proposal id into a
-- concrete one. Since we don't return a 'Gen' monad, the only source of
-- variability we have is the abstract proposal id.
elaborateProposalId ::
  Map Abstract.UpId Concrete.UpId ->
  Abstract.UpId ->
  Concrete.UpId
elaborateProposalId proposalsIdMap abstractProposalId =
  fromMaybe
    abstractIdHash
    (Map.lookup abstractProposalId proposalsIdMap)
  where
    -- If we cannot find a concrete proposal id that corresponds with the
    -- given abstract proposal id, then we return the (coerced) hash of the
    -- abstract proposal id.
    --
    -- NOTE: if the elaborators returned a `Gen a` value, then we could
    -- return random hashes here.
    abstractIdHash :: Concrete.UpId -- Keeps GHC happy ...
    abstractIdHash = coerce $ H.serializeCborHash id
      where
        Abstract.UpId id = abstractProposalId
