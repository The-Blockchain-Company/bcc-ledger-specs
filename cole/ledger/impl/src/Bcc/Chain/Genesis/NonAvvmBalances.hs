{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Chain.Genesis.NonAvvmBalances
  ( GenesisNonAvvmBalances (..),
    convertNonAvvmDataToBalances,
  )
where

import Bcc.Binary
  ( DecoderError,
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
  )
import Bcc.Chain.Common
  ( Address,
    Entropic,
    EntropicError,
    addEntropic,
    decodeAddressBase58,
    integerToEntropic,
  )
import Bcc.Prelude
import qualified Data.Map.Strict as M
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
  { unGenesisNonAvvmBalances :: Map Address Entropic
  }
  deriving (Show, Eq, NoThunks)

instance B.Buildable GenesisNonAvvmBalances where
  build (GenesisNonAvvmBalances m) =
    bprint ("GenesisNonAvvmBalances: " . mapJson) m

deriving instance Semigroup GenesisNonAvvmBalances

deriving instance Monoid GenesisNonAvvmBalances

instance Monad m => ToJSON m GenesisNonAvvmBalances where
  toJSON = toJSON . unGenesisNonAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisNonAvvmBalances where
  fromJSON = fmap GenesisNonAvvmBalances . fromJSON

instance ToCBOR GenesisNonAvvmBalances where
  toCBOR (GenesisNonAvvmBalances gnab) =
    encodeListLen 1
      <> toCBOR @(Map Address Entropic) gnab

instance FromCBOR GenesisNonAvvmBalances where
  fromCBOR = do
    enforceSize "GenesisNonAvvmBalances" 1
    GenesisNonAvvmBalances <$> fromCBOR @(Map Address Entropic)

data NonAvvmBalancesError
  = NonAvvmBalancesEntropicError EntropicError
  | NonAvvmBalancesDecoderError DecoderError

instance B.Buildable NonAvvmBalancesError where
  build = \case
    NonAvvmBalancesEntropicError err ->
      bprint
        ("Failed to construct a entropic in NonAvvmBalances.\n Error: " . build)
        err
    NonAvvmBalancesDecoderError err ->
      bprint
        ("Failed to decode NonAvvmBalances.\n Error: " . build)
        err

-- | Generate genesis address distribution out of avvm parameters. Txdistr of
--   the utxo is all empty. Redelegate it in calling function.
convertNonAvvmDataToBalances ::
  forall m.
  MonadError NonAvvmBalancesError m =>
  Map Text Integer ->
  m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = fmap GenesisNonAvvmBalances $ do
  converted <- traverse convert (M.toList balances)
  mkBalances converted
  where
    mkBalances :: [(Address, Entropic)] -> m (Map Address Entropic)
    mkBalances =
      -- Pull 'EntropicError's out of the 'Map' and lift them to
      -- 'NonAvvmBalancesError's
      (`wrapError` NonAvvmBalancesEntropicError)
        . sequence
        -- Make map joining duplicate keys with 'addEntropic' lifted from 'Entropic ->
        -- Entropic -> Either EntropicError Entropic' to 'Either EntropicError Entropic -> Either
        -- EntropicError Entropic -> Either EntropicError Entropic'
        . M.fromListWith (\c -> join . liftM2 addEntropic c)
        -- Lift the 'Entropic's to 'Either EntropicError Entropic's
        . fmap (second Right)

    convert :: (Text, Integer) -> m (Address, Entropic)
    convert (txt, i) = do
      addr <- decodeAddressBase58 txt `wrapError` NonAvvmBalancesDecoderError
      entropic <- integerToEntropic i `wrapError` NonAvvmBalancesEntropicError
      return (addr, entropic)
