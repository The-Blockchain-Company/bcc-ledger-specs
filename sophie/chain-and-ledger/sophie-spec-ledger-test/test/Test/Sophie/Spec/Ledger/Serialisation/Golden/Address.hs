{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Sophie.Spec.Ledger.Serialisation.Golden.Address
  ( tests,
    Sophie,
  )
where

import qualified Bcc.Chain.Common as Cole
import Bcc.Crypto.Hash (Hash (..), HashAlgorithm (..), hashFromBytes, sizeHash)
import Bcc.Crypto.Hash.Blake2b (Blake2b_224)
import Bcc.Ledger.Address
import Bcc.Ledger.BaseTypes (Network (..))
import Bcc.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import Bcc.Ledger.Crypto (StandardCrypto)
import Bcc.Ledger.Keys
  ( KeyRole (..),
    pattern KeyHash,
  )
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot (SlotNo (..))
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as LB16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Either
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Sophie.Spec.Ledger.Scripts (pattern ScriptHash)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

-- Crypto family as used in production Sophie
-- This should match that defined at https://github.com/The-Blockchain-Company/shardagnostic-network/blob/master/shardagnostic-consensus-sophie/src/Shardagnostic/Consensus/Sophie/Protocol/Crypto.hs

type SophieCrypto = StandardCrypto

type Sophie = SophieEra StandardCrypto

tests :: TestTree
tests =
  T.testGroup
    "Address golden tests"
    [ goldenTests_MockCrypto,
      goldenTests_SophieCrypto
    ]

{------------------------------------------------------------------------------
-- Golden tests
------------------------------------------------------------------------------}

goldenTests_MockCrypto :: TestTree
goldenTests_MockCrypto =
  T.testGroup
    "MockCrypto golden tests"
    [ golden "keyHash" putCredential keyHash "01020304",
      golden "scriptHash" putCredential scriptHash "05060708",
      golden "ptr" putPtr ptr "81000203",
      golden
        "addrBaseKK"
        putAddr
        (Addr Testnet keyHash (StakeRefBase keyHash))
        "000102030401020304",
      golden
        "addrBaseSK"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase keyHash))
        "100506070801020304",
      golden
        "addrBaseKS"
        putAddr
        (Addr Testnet keyHash (StakeRefBase scriptHash))
        "200102030405060708",
      golden
        "addrBaseSS"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase scriptHash))
        "300506070805060708",
      golden
        "addrPtrK"
        putAddr
        (Addr Testnet keyHash (StakeRefPtr ptr))
        "400102030481000203",
      golden
        "addrPtrS"
        putAddr
        (Addr Testnet scriptHash (StakeRefPtr ptr))
        "500506070881000203",
      golden
        "addrEnterpriseK"
        putAddr
        (Addr Testnet keyHash StakeRefNull)
        "6001020304",
      golden
        "addrEnterpriseS"
        putAddr
        (Addr Testnet scriptHash StakeRefNull)
        "7005060708",
      golden
        "rewardAcntK"
        putRewardAcnt
        (RewardAcnt Testnet keyHash)
        "e001020304",
      golden
        "rewardAcntS"
        putRewardAcnt
        (RewardAcnt Testnet scriptHash)
        "f005060708"
    ]
  where
    keyHash :: Credential kh C_Crypto
    keyHash =
      KeyHashObj . KeyHash . UnsafeHash $
        SBS.toShort . fromRight (error "Unable to decode") . B16.decode $ "01020304"
    scriptHash :: Credential kh C_Crypto
    scriptHash =
      ScriptHashObj . ScriptHash . UnsafeHash $
        SBS.toShort . fromRight (error "Unable to decode") . B16.decode $ "05060708"
    ptr :: Ptr
    ptr = Ptr (SlotNo 128) 2 3

goldenTests_SophieCrypto :: TestTree
goldenTests_SophieCrypto =
  T.testGroup
    "SophieCrypto golden tests"
    [ golden
        "addrEnterpriseK for network id = 0"
        putAddr
        (Addr Testnet paymentKey StakeRefNull)
        "608a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      golden
        "addrBaseKK for network id = 0"
        putAddr
        (Addr Testnet paymentKey (StakeRefBase stakeKey))
        "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "addrPtrK for network id = 0"
        putAddr
        (Addr Testnet paymentKey (StakeRefPtr ptr))
        "408a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203",
      golden
        "addrEnterpriseK for network id = 1"
        putAddr
        (Addr Mainnet paymentKey StakeRefNull)
        "618a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      golden
        "addrBaseKK for network id = 1"
        putAddr
        (Addr Mainnet paymentKey (StakeRefBase stakeKey))
        "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "addrPtrK for network id = 1"
        putAddr
        (Addr Mainnet paymentKey (StakeRefPtr ptr))
        "418a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203",
      golden
        "rewardAcntK"
        putRewardAcnt
        (RewardAcnt Testnet stakeKey)
        "e008b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      golden
        "bootstrapAddr for network id = 1"
        putAddr
        ( AddrBootstrap . BootstrapAddress $
            Cole.Address
              { Cole.addrRoot = read "4bf3c2ee56bfef278d65f7388c46efa12a1069698e474f77adf0cf6a",
                Cole.addrAttributes =
                  Cole.Attributes
                    { Cole.attrData =
                        Cole.AddrAttributes
                          { Cole.aaVKDerivationPath = Nothing,
                            Cole.aaNetworkMagic = Cole.NetworkMainOrStage
                          },
                      Cole.attrRemain = Cole.UnparsedFields mempty
                    },
                Cole.addrType = Cole.ATVerKey
              }
        )
        "82d818582183581c4bf3c2ee56bfef278d65f7388c46efa12a1069698e474f77adf0cf6aa0001ab4aad9a5"
    ]
  where
    paymentKey :: Credential 'Payment SophieCrypto
    paymentKey = keyBlake2b224 $ B16.encode "1a2a3a4a5a6a7a8a"
    stakeKey :: Credential 'Staking SophieCrypto
    stakeKey = keyBlake2b224 $ B16.encode "1c2c3c4c5c6c7c8c"
    ptr :: Ptr
    ptr = Ptr (SlotNo 128) 2 3
    -- 32-byte verification key is expected, vk, ie., public key without chain code.
    -- The verification key undergoes Blake2b_224 hashing
    -- and should be 28-byte in the aftermath
    keyBlake2b224 :: BS.ByteString -> Credential kh SophieCrypto
    keyBlake2b224 vk =
      KeyHashObj . KeyHash . fromJust . hashFromBytes $ hk
      where
        hash = digest (Proxy :: Proxy Blake2b_224)
        vk' = invariantSize 32 vk
        hk =
          invariantSize
            (fromIntegral $ sizeHash (Proxy :: Proxy Blake2b_224))
            (hash vk')
    invariantSize :: HasCallStack => Int -> BS.ByteString -> BS.ByteString
    invariantSize expectedLength bytes
      | BS.length bytes == expectedLength = bytes
      | otherwise =
        error $
          "length was "
            ++ show (BS.length bytes)
            ++ ", but expected to be "
            ++ show expectedLength

golden :: String -> (a -> B.Put) -> a -> LBS.ByteString -> TestTree
golden name put value expected =
  T.testCase name $
    T.assertEqual name expected (LB16.encode . B.runPut . put $ value)
