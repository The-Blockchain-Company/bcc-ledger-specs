{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bcc.Chain.Genesis.Spec
  ( GenesisSpec (..),
    mkGenesisSpec,
  )
where

import Bcc.Chain.Common (BlockCount)
import Bcc.Chain.Genesis.AvvmBalances (GenesisAvvmBalances (..))
import Bcc.Chain.Genesis.Delegation (GenesisDelegation (..))
import Bcc.Chain.Genesis.Initializer (GenesisInitializer (..))
import Bcc.Chain.Update.ProtocolParameters (ProtocolParameters)
import Bcc.Crypto (ProtocolMagic)
import Bcc.Prelude
import Data.List (nub)
import qualified Data.Map.Strict as M

-- | Specification how to generate full 'GenesisData'
data GenesisSpec = UnsafeGenesisSpec
  { -- | Genesis data describes avvm utxo
    gsAvvmDistr :: !GenesisAvvmBalances,
    -- | Genesis state of heavyweight delegation. Will be concatenated with
    --   delegation genesis keyHashes if 'tiUseHeavyDlg' is 'True'
    gsHeavyDelegation :: !GenesisDelegation,
    -- | Genesis 'ProtocolParameters'
    gsProtocolParameters :: !ProtocolParameters,
    -- | The security parameter of the Shardagnostic protocol
    gsK :: !BlockCount,
    -- | The magic number unique to any instance of Bcc
    gsProtocolMagic :: !ProtocolMagic,
    -- | Other data which depend on genesis type
    gsInitializer :: !GenesisInitializer
  }
  deriving (Eq, Show, Generic)

-- | Safe constructor for 'GenesisSpec'. Throws error if something
-- goes wrong.
mkGenesisSpec ::
  GenesisAvvmBalances ->
  GenesisDelegation ->
  ProtocolParameters ->
  BlockCount ->
  ProtocolMagic ->
  GenesisInitializer ->
  Either Text GenesisSpec
mkGenesisSpec avvmDistr delega bvd k pm specType = do
  let avvmKeys = M.keys $ unGenesisAvvmBalances avvmDistr
  (length (nub avvmKeys) == length avvmKeys)
    `orThrowError` "mkGenesisSpec: there are duplicates in avvm balances"
  -- All checks passed
  pure $ UnsafeGenesisSpec avvmDistr delega bvd k pm specType
