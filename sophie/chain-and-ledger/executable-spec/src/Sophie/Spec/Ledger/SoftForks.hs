{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Sophie.Spec.Ledger.SoftForks
  ( validMetadata,
    restrictPoolMetadataHash,
  )
where

import GHC.Records
import Sophie.Spec.Ledger.PParams (ProtVer (..))

validMetadata ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
validMetadata pp = getField @"_protocolVersion" pp > ProtVer 2 0

restrictPoolMetadataHash ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
restrictPoolMetadataHash pp = getField @"_protocolVersion" pp > ProtVer 4 0
