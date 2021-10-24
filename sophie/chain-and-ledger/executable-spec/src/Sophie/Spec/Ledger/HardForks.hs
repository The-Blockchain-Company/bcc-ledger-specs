{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Sophie.Spec.Ledger.HardForks
  ( aggregatedRewards,
    allowMIRTransfer,
    validatePoolRewardAccountNetID,
    allowScriptStakeCredsToEarnRewards,
    translateTimeForZerepochScripts,
    sentryRewards,
  )
where

import GHC.Records
import Numeric.Natural (Natural)
import Sophie.Spec.Ledger.PParams (ProtVer (..))

aggregatedRewards ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
aggregatedRewards pp = pvMajor (getField @"_protocolVersion" pp) > 2

sentryRewards ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
sentryRewards pp = pvSentry (getField @"_protocolVersion" pp) > 0 

-- | Starting with protocol version 5, the MIR certs will also be
-- able to transfer funds between the reserves and the treasury.
-- Additionally, the semantics for the pervious functionality will
-- change a bit. Before version 5 redundancies in the instantaneous
-- reward mapping were handled by overriding. Now they are handled
-- by adding the values and allowing for negatives updates, provided
-- the sum for each key remains positive.
allowMIRTransfer ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
allowMIRTransfer pp = pvMajor (getField @"_protocolVersion" pp) > 4

-- | Starting with protocol version 5, we will validate the network ID
-- for the reward account listed in stake pool registration certificates.
validatePoolRewardAccountNetID ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
validatePoolRewardAccountNetID pp = pvMajor (getField @"_protocolVersion" pp) > 4

-- | Starting with protocol version 5, Stake Credentials bound by scripts
-- will be eligibile for staking rewards.
allowScriptStakeCredsToEarnRewards ::
  Natural ->
  Bool
allowScriptStakeCredsToEarnRewards pvM = pvM > 4

-- | Starting with protocol version 6, we translate slots to time correctly for
-- Zerepoch scripts.
translateTimeForZerepochScripts ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
translateTimeForZerepochScripts pp = pvMajor (getField @"_protocolVersion" pp) > 5
