{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Bcc.Chain.Genesis.Hash
  ( GenesisHash (..),
  )
where

import Bcc.Binary (FromCBOR, Raw, ToCBOR)
import Bcc.Crypto.Hashing (Hash)
import Bcc.Prelude
import Data.Aeson (ToJSON)
import NoThunks.Class (NoThunks (..))

newtype GenesisHash = GenesisHash
  { unGenesisHash :: Hash Raw
  }
  deriving (Eq, Generic, NFData, FromCBOR, ToCBOR, NoThunks)

deriving instance Show GenesisHash

-- Used for debugging purposes only
instance ToJSON GenesisHash
