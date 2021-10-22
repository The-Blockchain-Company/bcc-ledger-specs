-- | Dummy values used in tests (replacing `configuration.yaml`)
module Test.Bcc.Crypto.Dummy
  ( annotatedProtocolMagicId,
    aProtocolMagic,
    protocolMagic,
    protocolMagicId,
  )
where

import Bcc.Binary (Annotated (..), serialize')
import Bcc.Crypto
  ( AProtocolMagic (..),
    ProtocolMagic,
    ProtocolMagicId (..),
    RequiresNetworkMagic (..),
  )
import Bcc.Prelude

aProtocolMagic :: AProtocolMagic ByteString
aProtocolMagic = AProtocolMagic annotatedProtocolMagicId RequiresMagic

protocolMagic :: ProtocolMagic
protocolMagic = AProtocolMagic (Annotated protocolMagicId ()) RequiresMagic

annotatedProtocolMagicId :: Annotated ProtocolMagicId ByteString
annotatedProtocolMagicId =
  Annotated protocolMagicId (serialize' protocolMagicId)

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 55550001
