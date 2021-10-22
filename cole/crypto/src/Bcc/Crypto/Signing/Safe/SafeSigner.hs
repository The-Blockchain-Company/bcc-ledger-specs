module Bcc.Crypto.Signing.Safe.SafeSigner
  ( SafeSigner (..),
    noPassSafeSigner,
    safeToVerification,
  )
where

import Bcc.Crypto.Signing.Safe.PassPhrase (PassPhrase, emptyPassphrase)
import Bcc.Crypto.Signing.SigningKey (SigningKey (..), toVerification)
import Bcc.Crypto.Signing.VerificationKey (VerificationKey (..))
import Bcc.Prelude

-- | SafeSigner datatype to encapsulate sensitive data
data SafeSigner = SafeSigner !SigningKey !PassPhrase
  deriving (Show)

noPassSafeSigner :: SigningKey -> SafeSigner
noPassSafeSigner sk = SafeSigner sk emptyPassphrase

safeToVerification :: SafeSigner -> VerificationKey
safeToVerification (SafeSigner sk _) = toVerification sk
