{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Chain.Genesis.AvvmBalances
  ( GenesisAvvmBalances (..),
  )
where

import Bcc.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
  )
import Bcc.Chain.Common (Entropic)
import Bcc.Crypto.Signing.Redeem (CompactRedeemVerificationKey)
import Bcc.Prelude
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | Predefined balances of AVVM (Bcc Voucher Vending Machine) entries.
-- People who purchased Bcc at a pre-sale were issued a certificate during
-- the pre-sale period. These certificates allow customers to redeem BCC.
newtype GenesisAvvmBalances = GenesisAvvmBalances
  { unGenesisAvvmBalances :: Map CompactRedeemVerificationKey Entropic
  }
  deriving (Show, Eq, Semigroup, NoThunks)

instance Monad m => ToJSON m GenesisAvvmBalances where
  toJSON = toJSON . unGenesisAvvmBalances

instance MonadError SchemaError m => FromJSON m GenesisAvvmBalances where
  fromJSON = fmap (GenesisAvvmBalances . forceElemsToWHNF) . fromJSON

instance ToCBOR GenesisAvvmBalances where
  toCBOR (GenesisAvvmBalances gab) =
    encodeListLen 1
      <> toCBOR @(Map CompactRedeemVerificationKey Entropic) gab

instance FromCBOR GenesisAvvmBalances where
  fromCBOR = do
    enforceSize "GenesisAvvmBalances" 1
    GenesisAvvmBalances <$> fromCBOR @(Map CompactRedeemVerificationKey Entropic)
