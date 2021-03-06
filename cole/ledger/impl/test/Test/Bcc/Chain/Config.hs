{-# LANGUAGE OverloadedStrings #-}

module Test.Bcc.Chain.Config
  ( readMainetCfg,
  )
where

import Bcc.Binary (Raw)
import qualified Bcc.Chain.Genesis as Genesis
import Bcc.Crypto.Hashing (Hash, decodeHash)
import Bcc.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import Bcc.Prelude

-- | Read the test mainnet configuration file from the @test@ directory.
--
-- An error is thrown if it is not possible to elaborate a genesis
-- configuration from the genesis file.
--
-- We use `RequiresNoMagic`, as it indicates mainnet
readMainetCfg :: MonadIO m => m Genesis.Config
readMainetCfg = do
  let genHash =
        either
          (panic . show . Genesis.GenesisHashDecodeError)
          identity
          ( decodeHash
              "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
          ) ::
          Hash Raw

  either (panic . show) identity
    <$> runExceptT
      (Genesis.mkConfigFromFile RequiresNoMagic "mainnet-genesis.json" genHash)
