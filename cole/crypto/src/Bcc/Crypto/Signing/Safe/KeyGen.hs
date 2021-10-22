module Bcc.Crypto.Signing.Safe.KeyGen
  ( safeDeterministicKeyGen,
    safeKeyGen,
  )
where

import Bcc.Crypto.Signing.Safe.PassPhrase (PassPhrase (..))
import Bcc.Crypto.Signing.SigningKey (SigningKey (..))
import Bcc.Crypto.Signing.VerificationKey (VerificationKey (..))
import qualified Bcc.Crypto.Wallet as CC
import Bcc.Prelude
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.ByteString as BS

safeCreateKeypairFromSeed :: BS.ByteString -> PassPhrase -> (CC.XPub, CC.XPrv)
safeCreateKeypairFromSeed seed (PassPhrase pp) =
  let prv = CC.generate seed pp in (CC.toXPub prv, prv)

-- NB. It's recommended to run it with 'runSecureRandom' from
-- "Bcc.Crypto.Random" because the OpenSSL generator is probably safer than
-- the default IO generator.
safeKeyGen :: (MonadRandom m) => PassPhrase -> m (VerificationKey, SigningKey)
safeKeyGen pp = do
  seed <- getRandomBytes 32
  pure $ safeDeterministicKeyGen seed pp

safeDeterministicKeyGen ::
  BS.ByteString -> PassPhrase -> (VerificationKey, SigningKey)
safeDeterministicKeyGen seed pp =
  bimap VerificationKey SigningKey (safeCreateKeypairFromSeed seed pp)
