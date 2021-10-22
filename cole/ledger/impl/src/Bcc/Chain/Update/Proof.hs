module Bcc.Chain.Update.Proof
  ( Proof,
    mkProof,
    recoverProof,
  )
where

import Bcc.Chain.Update.Payload (APayload (..), Payload)
import Bcc.Crypto (Hash, hashDecoded, serializeCborHash)
import Bcc.Prelude

-- | Proof that body of update message contains 'Update.Payload'
type Proof = Hash Payload

mkProof :: Payload -> Proof
mkProof = serializeCborHash

recoverProof :: APayload ByteString -> Proof
recoverProof = hashDecoded
