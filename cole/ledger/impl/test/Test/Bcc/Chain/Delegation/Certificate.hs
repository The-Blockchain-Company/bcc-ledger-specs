{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Chain.Delegation.Certificate
  ( tests,
  )
where

import Bcc.Binary (decodeFull, serialize, slice)
import Bcc.Chain.Delegation
  ( ACertificate (delegateVK),
    Certificate,
    isValid,
    signCertificate,
  )
import Bcc.Prelude
import qualified Data.ByteString.Lazy as BSL
import Hedgehog (Group, Property, assert, discover, forAll, property)
import qualified Hedgehog.Gen as Gen
import Test.Bcc.Chain.Slotting.Gen (genEpochNumber)
import qualified Test.Bcc.Crypto.Dummy as Dummy
import Test.Bcc.Crypto.Gen (genSafeSigner, genVerificationKey)

--------------------------------------------------------------------------------
-- Test Group
--------------------------------------------------------------------------------

tests :: Group
tests = $$discover

--------------------------------------------------------------------------------
-- Certificate Properties
--------------------------------------------------------------------------------

-- | Can validate 'Certificate's produced by 'signCertificate'
prop_certificateCorrect :: Property
prop_certificateCorrect = property $ do
  cert <-
    forAll $
      signCertificate Dummy.protocolMagicId
        <$> genVerificationKey
        <*> genEpochNumber
        <*> genSafeSigner

  let aCert = annotateCert cert

  assert $ isValid Dummy.annotatedProtocolMagicId aCert

-- | Cannot validate 'Certificate's with incorrect verification keys
prop_certificateIncorrect :: Property
prop_certificateIncorrect = property $ do
  cert <-
    forAll $
      signCertificate Dummy.protocolMagicId
        <$> genVerificationKey
        <*> genEpochNumber
        <*> genSafeSigner
  badDelegateVK <- forAll $ Gen.filter (/= delegateVK cert) genVerificationKey

  let badCert = cert {delegateVK = badDelegateVK}
      aBadCert = annotateCert badCert

  assert . not $ isValid Dummy.annotatedProtocolMagicId aBadCert

annotateCert :: Certificate -> ACertificate ByteString
annotateCert cert =
  fmap (BSL.toStrict . slice bytes)
    . fromRight
      (panic "prop_certificateCorrect: Round trip broken for Certificate")
    $ decodeFull bytes
  where
    bytes = serialize cert
