{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Chain.Common.KeyHash
  ( KeyHash (..),
    hashKey,
  )
where

import Bcc.Binary (FromCBOR, ToCBOR)
import Bcc.Chain.Common.AddressHash
import Bcc.Crypto (decodeAbstractHash, hashHexF)
import Bcc.Crypto.Signing (VerificationKey)
import Bcc.Prelude
import Formatting (formatToString)
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical
  ( FromObjectKey (..),
    JSValue (..),
    ToObjectKey (..),
    toJSString,
  )

-- | A 'KeyHash' refers to a 'VerificationKey'
newtype KeyHash = KeyHash
  { unKeyHash :: AddressHash VerificationKey
  }
  deriving
    ( Eq,
      Ord,
      Show,
      NFData,
      Buildable,
      FromCBOR,
      ToCBOR,
      HeapWords,
      NoThunks
    )

instance Monad m => ToObjectKey m KeyHash where
  toObjectKey = pure . toJSString . formatToString hashHexF . unKeyHash

instance MonadError SchemaError m => FromObjectKey m KeyHash where
  fromObjectKey =
    fmap (Just . KeyHash)
      . parseJSString decodeAbstractHash
      . JSString

hashKey :: VerificationKey -> KeyHash
hashKey = KeyHash . addressHash
