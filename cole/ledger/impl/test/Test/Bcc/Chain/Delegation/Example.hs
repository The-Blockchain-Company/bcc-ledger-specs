module Test.Bcc.Chain.Delegation.Example
  ( exampleCertificates,
  )
where

import Bcc.Chain.Delegation (Certificate, signCertificate)
import Bcc.Chain.Slotting (EpochNumber (..))
import Bcc.Crypto (ProtocolMagicId (..))
import Bcc.Prelude
import Data.List (zipWith4)
import Test.Bcc.Crypto.Example (exampleVerificationKeys, staticSafeSigners)

staticProtocolMagics :: [ProtocolMagicId]
staticProtocolMagics = ProtocolMagicId <$> [0 .. 5]

exampleCertificates :: [Certificate]
exampleCertificates =
  zipWith4
    signCertificate
    staticProtocolMagics
    (exampleVerificationKeys 1 6)
    exampleEpochIndices
    staticSafeSigners
  where
    exampleEpochIndices = EpochNumber <$> [5, 1, 3, 27, 99, 247]
