{-# LANGUAGE NamedFieldPuns #-}

module Bcc.Chain.Update.Validation.Interface.ProtocolVersionBump
  ( Environment (..),
    State (..),
    tryBumpVersion,
  )
where

import Bcc.Chain.Common.BlockCount (BlockCount)
import Bcc.Chain.ProtocolConstants (kUpdateStabilityParam)
import Bcc.Chain.Slotting (SlotNumber, subSlotCount)
import Bcc.Chain.Update.ProtocolParameters (ProtocolParameters)
import Bcc.Chain.Update.ProtocolVersion (ProtocolVersion)
import Bcc.Chain.Update.Validation.Endorsement
  ( CandidateProtocolUpdate (CandidateProtocolUpdate),
    cpuProtocolParameters,
    cpuProtocolVersion,
    cpuSlot,
  )
import Bcc.Prelude hiding (State)

data Environment = Environment
  { k :: !BlockCount,
    epochFirstSlot :: !SlotNumber,
    candidateProtocolVersions :: ![CandidateProtocolUpdate]
  }

data State = State
  { nextProtocolVersion :: !ProtocolVersion,
    nextProtocolParameters :: !ProtocolParameters
  }

-- | Change the protocol version when an epoch change is detected, and there is
-- a candidate protocol update that was confirmed at least @4 * k@ slots before
-- the start of the new epoch, where @k@ is the chain security parameter.
--
-- For a full history of why this is required, see
-- https://github.com/The-Blockchain-Company/bcc-ledger-specs/issues/1288
--
-- This corresponds to the @PVBUMP@ rules in the Cole ledger specification.
tryBumpVersion ::
  Environment ->
  State ->
  State
tryBumpVersion env st =
  case stableCandidates of
    (newestStable : _) ->
      let CandidateProtocolUpdate
            { cpuProtocolVersion,
              cpuProtocolParameters
            } = newestStable
       in st
            { nextProtocolVersion = cpuProtocolVersion,
              nextProtocolParameters = cpuProtocolParameters
            }
    _ -> st
  where
    Environment {k, epochFirstSlot, candidateProtocolVersions} = env

    stableCandidates =
      filter ((<= subSlotCount (kUpdateStabilityParam k) epochFirstSlot) . cpuSlot) candidateProtocolVersions
