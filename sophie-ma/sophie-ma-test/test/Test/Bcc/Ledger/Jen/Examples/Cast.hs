{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Test.Bcc.Ledger.Jen.Examples.Cast
-- Description : Cast of characters for Jen ledger examples
--
-- The cast of Characters for Jen Ledger Examples
module Test.Bcc.Ledger.Jen.Examples.Cast
  ( alicePay,
    aliceStake,
    aliceAddr,
    bobPay,
    bobStake,
    bobAddr,
    carlPay,
    carlStake,
    carlAddr,
    dariaPay,
    dariaStake,
    dariaAddr,
  )
where

import Bcc.Ledger.Address (Addr (..))
import Bcc.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
  )
import Test.Bcc.Ledger.EraBuffet (TestCrypto)
import Test.Sophie.Spec.Ledger.VestedSealUtils (RawSeed (..), mkAddr, mkKeyPair)

-- | Alice's payment key pair
alicePay :: KeyPair 'Payment TestCrypto
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

-- | Alice's stake key pair
aliceStake :: KeyPair 'Staking TestCrypto
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

-- | Alice's base address
aliceAddr :: Addr TestCrypto
aliceAddr = mkAddr (alicePay, aliceStake)

-- | Bob's payment key pair
bobPay :: KeyPair 'Payment TestCrypto
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 2 2 2 2 2)

-- | Bob's stake key pair
bobStake :: KeyPair 'Staking TestCrypto
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 3 3 3 3 3)

-- | Bob's address
bobAddr :: Addr TestCrypto
bobAddr = mkAddr (bobPay, bobStake)

-- Carl's payment key pair
carlPay :: KeyPair 'Payment TestCrypto
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 4 4 4 4 4)

-- | Carl's stake key pair
carlStake :: KeyPair 'Staking TestCrypto
carlStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 5 5 5 5 5)

-- | Carl's address
carlAddr :: Addr TestCrypto
carlAddr = mkAddr (carlPay, carlStake)

-- | Daria's payment key pair
dariaPay :: KeyPair 'Payment TestCrypto
dariaPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 6 6 6 6 6)

-- | Daria's stake key pair
dariaStake :: KeyPair 'Staking TestCrypto
dariaStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 7 7 7 7 7)

-- | Daria's address
dariaAddr :: Addr TestCrypto
dariaAddr = mkAddr (dariaPay, dariaStake)
