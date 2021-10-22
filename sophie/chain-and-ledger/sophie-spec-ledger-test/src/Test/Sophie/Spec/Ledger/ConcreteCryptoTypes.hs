{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Sophie.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
    ExMock,
    C_Crypto,
    C,
    TestCrypto,
    StandardCrypto,
  )
where

import Bcc.Crypto.DSIGN (MockDSIGN, VerKeyDSIGN)
import qualified Bcc.Crypto.DSIGN.Class as DSIGN
import Bcc.Crypto.Hash (Blake2bPrefix)
import Bcc.Crypto.KES (MockKES)
import qualified Bcc.Crypto.KES.Class as KES
import Bcc.Crypto.Util (SignableRepresentation)
import qualified Bcc.Crypto.VRF as VRF
import Bcc.Ledger.BaseTypes (Seed)
import Bcc.Ledger.Crypto
import Bcc.Ledger.Sophie (SophieEra)
import Sophie.Spec.Ledger.API (OptimumCrypto)
import Test.Bcc.Crypto.VRF.Fake (FakeVRF)

-- | Mocking constraints used in generators
type Mock c =
  ( OptimumCrypto c,
    KES.Signable (KES c) ~ SignableRepresentation,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation,
    VRF.Signable (VRF c) Seed
  )

-- | Additional mocking constraints used in examples.
type ExMock c =
  ( Mock c,
    Num (DSIGN.SignKeyDSIGN (DSIGN c)),
    Num (VerKeyDSIGN (DSIGN c)),
    VRF c ~ FakeVRF
  )

type C = SophieEra C_Crypto

type TestCrypto = C_Crypto

data C_Crypto

instance Bcc.Ledger.Crypto.Crypto C_Crypto where
  type HASH C_Crypto = Blake2bPrefix 10
  type ADDRHASH C_Crypto = Blake2bPrefix 8
  type DSIGN C_Crypto = MockDSIGN
  type KES C_Crypto = MockKES 10
  type VRF C_Crypto = FakeVRF

instance OptimumCrypto C_Crypto
