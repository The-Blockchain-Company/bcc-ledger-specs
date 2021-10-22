import Bcc.Prelude
import qualified Test.Bcc.Crypto.CBOR
import qualified Test.Bcc.Crypto.Hashing
import qualified Test.Bcc.Crypto.Json
import qualified Test.Bcc.Crypto.Keys
import qualified Test.Bcc.Crypto.Limits
import qualified Test.Bcc.Crypto.Random
import qualified Test.Bcc.Crypto.Signing.Redeem
import qualified Test.Bcc.Crypto.Signing.Redeem.Compact
import qualified Test.Bcc.Crypto.Signing.Safe
import qualified Test.Bcc.Crypto.Signing.Signing
import Test.Bcc.Prelude

-- | Main testing action
main :: IO ()
main =
  runTests
    [ Test.Bcc.Crypto.CBOR.tests,
      Test.Bcc.Crypto.Hashing.tests,
      Test.Bcc.Crypto.Json.tests,
      Test.Bcc.Crypto.Keys.tests,
      Test.Bcc.Crypto.Limits.tests,
      Test.Bcc.Crypto.Random.tests,
      Test.Bcc.Crypto.Signing.Redeem.tests,
      Test.Bcc.Crypto.Signing.Redeem.Compact.tests,
      Test.Bcc.Crypto.Signing.Safe.tests,
      Test.Bcc.Crypto.Signing.Signing.tests
    ]
