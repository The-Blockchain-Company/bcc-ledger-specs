{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sophie.Spec.Ledger.Address.Bootstrap
  ( genBootstrapAddress,
    testBootstrapSpending,
    testBootstrapNotSpending,
    bootstrapHashTest,
    genSignature,
  )
where

import Bcc.Binary (serialize')
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Crypto.DSIGN as DSIGN
import qualified Bcc.Crypto.Hash as Hash
import qualified Bcc.Crypto.Signing as Cole
import qualified Bcc.Crypto.Wallet as Cole
import Bcc.Ledger.Address
  ( Addr (..),
    BootstrapAddress (..),
    bootstrapKeyHash,
  )
import Bcc.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
  )
import Bcc.Ledger.Coin (Coin (..))
import Bcc.Ledger.Credential
  ( Credential (..),
    StakeReference (..),
  )
import Bcc.Ledger.Crypto (Crypto (..))
import Bcc.Ledger.Keys
  ( GenDelegs (..),
    KeyRole (..),
    VKey (..),
    coerceKeyRole,
    hashKey,
  )
import Bcc.Ledger.SafeHash (extractHash, hashAnnotated)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot
  ( SlotNo (..),
  )
import Bcc.Ledger.Val ((<->))
import Bcc.Prelude
  ( ByteString,
  )
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Sophie.Spec.Ledger.Address.Bootstrap
import Sophie.Spec.Ledger.LedgerState
  ( PPUPState (..),
    UTxOState (..),
  )
import Sophie.Spec.Ledger.PParams
  ( PParams' (..),
    ProposedPPUpdates (..),
    emptyPParams,
  )
import Sophie.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Sophie.Spec.Ledger.STS.Utxow
  ( UTXOW,
    UtxowPredicateFailure (..),
  )
import Sophie.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Sophie.Spec.Ledger.TxBody
  ( TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Sophie.Spec.Ledger.UTxO
  ( UTxO (..),
  )
import qualified Test.Bcc.Chain.Common.Gen as Cole
import qualified Test.Bcc.Crypto.Gen as Cole
import Test.Bcc.Prelude (genBytes)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Hedgehog (hedgehog)
import qualified Test.Sophie.Spec.Ledger.ConcreteCryptoTypes as Original
  ( C_Crypto,
  )
import Test.Sophie.Spec.Ledger.Generator.EraGen (genesisId)
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Test.Sophie.Spec.Ledger.Orphans ()
import Test.Sophie.Spec.Ledger.VestedSealUtils (testSTS)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
  ( Assertion,
  )
import Test.Tasty.QuickCheck (testProperty, (===))

bootstrapHashTest :: TestTree
bootstrapHashTest = testProperty "rebuild the 'addr root' using a bootstrap witness" $
  do
    (coleVKey, coleAddr) <- genColeVKeyAddr
    sig <- genSignature
    let addr = BootstrapAddress coleAddr
        (sophieVKey, chainCode) = unpackColeVKey @C_crypto coleVKey
        witness :: BootstrapWitness C_crypto
        witness =
          BootstrapWitness
            { bwKey = sophieVKey,
              bwChainCode = chainCode,
              bwSig = sig,
              bwAttributes = serialize' $ Cole.addrAttributes coleAddr
            }
    pure $
      (coerceKeyRole $ bootstrapKeyHash @C_crypto addr)
        === bootstrapWitKeyHash witness

genSignature :: forall a b. DSIGN.DSIGNAlgorithm a => Gen (DSIGN.SignedDSIGN a b)
genSignature =
  DSIGN.SignedDSIGN
    . fromJust
    . DSIGN.rawDeserialiseSigDSIGN
    <$> hedgehog (genBytes . fromIntegral $ DSIGN.sizeSigDSIGN ([] @a))

genBootstrapAddress :: Gen (BootstrapAddress crypto)
genBootstrapAddress = BootstrapAddress . snd <$> genColeVKeyAddr

genColeVKeyAddr :: Gen (Cole.VerificationKey, Cole.Address)
genColeVKeyAddr = do
  vkey <- hedgehog Cole.genVerificationKey
  addr <- genColeAddrFromVKey vkey
  pure (vkey, addr)

genColeAddrFromVKey :: Cole.VerificationKey -> Gen Cole.Address
genColeAddrFromVKey vkey =
  Cole.makeAddress (Cole.VerKeyASD vkey) <$> hedgehog Cole.genAddrAttributes

utxo0 :: UTxO C
utxo0 =
  UTxO $
    Map.singleton
      (TxIn genesisId 0)
      (TxOut aliceAddr aliceInitCoin)

utxoState0 :: UTxOState C
utxoState0 =
  UTxOState
    { _utxo = utxo0,
      _deposited = Coin 0,
      _fees = Coin 0,
      _ppups = PPUPState (ProposedPPUpdates mempty) (ProposedPPUpdates mempty)
    }

tx :: Tx C
tx = Tx txBody mempty {bootWits = Set.fromList [aliceWitness]} SNothing

txBad :: Tx C
txBad = Tx txBody mempty {bootWits = Set.fromList [aliceBadWitness]} SNothing

utxoState1 :: UTxOState C
utxoState1 =
  UTxOState
    { _utxo = UTxO $ Map.fromList [bobResult, aliceResult],
      _deposited = Coin 0,
      _fees = Coin 10,
      _ppups = PPUPState (ProposedPPUpdates mempty) (ProposedPPUpdates mempty)
    }
  where
    txid = TxId $ hashAnnotated txBody
    bobResult = (TxIn txid 0, TxOut bobAddr coinsToBob)
    aliceResult = (TxIn txid 1, TxOut aliceAddr (Coin 998990))

utxoEnv :: UtxoEnv C
utxoEnv =
  UtxoEnv
    0
    emptyPParams {_maxTxSize = 1000}
    mempty
    (GenDelegs mempty)

aliceInitCoin :: Coin
aliceInitCoin = Coin 1000000

aliceSigningKey :: Cole.SigningKey
aliceSigningKey = Cole.SigningKey $ Cole.generate seed (mempty :: ByteString)
  where
    seed :: ByteString -- 32 bytes
    seed = "12345678901234567890123456789012"

aliceVKey :: VKey 'Witness C_crypto
aliceVKey = fst . unpackColeVKey . Cole.toVerification $ aliceSigningKey

aliceColeAddr :: Cole.Address
aliceColeAddr = Cole.makeAddress asd attrs
  where
    asd = Cole.VerKeyASD $ coleVerificationKey
    attrs =
      Cole.AddrAttributes
        (Just (Cole.HDAddressPayload "a compressed lenna.png"))
        (Cole.NetworkTestnet 0)
    coleVerificationKey = Cole.toVerification aliceSigningKey

aliceAddr :: Addr C_crypto
aliceAddr = AddrBootstrap (BootstrapAddress aliceColeAddr)

aliceWitness :: BootstrapWitness C_crypto
aliceWitness =
  makeBootstrapWitness
    (extractHash (hashAnnotated txBody))
    aliceSigningKey
    (Cole.addrAttributes aliceColeAddr)

aliceBadWitness :: BootstrapWitness C_crypto
aliceBadWitness =
  makeBootstrapWitness
    (extractHash (hashAnnotated txBody {_ttl = SlotNo 100000000}))
    aliceSigningKey
    (Cole.addrAttributes aliceColeAddr)

bobAddr :: Addr C_crypto
bobAddr = Addr Testnet (KeyHashObj k) StakeRefNull
  where
    k = coerceKeyRole $ hashKey aliceVKey

coinsToBob :: Coin
coinsToBob = Coin 1000

txBody :: TxBody C
txBody =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut bobAddr coinsToBob, TxOut aliceAddr change],
      _certs = StrictSeq.fromList mempty,
      _wdrls = Wdrl Map.empty,
      _txfee = fee,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }
  where
    change = (aliceInitCoin <-> coinsToBob) <-> fee
    fee = Coin 10

testBootstrapSpending :: Assertion
testBootstrapSpending =
  testSTS @(UTXOW C)
    utxoEnv
    utxoState0
    tx
    (Right utxoState1)

testBootstrapNotSpending :: Assertion
testBootstrapNotSpending =
  testSTS @(UTXOW C)
    utxoEnv
    utxoState0
    txBad
    (Left [InvalidWitnessesUTXOW [aliceVKey]])

type C = SophieEra C_crypto

data C_crypto

instance Bcc.Ledger.Crypto.Crypto C_crypto where
  type KES C_crypto = KES Original.C_Crypto
  type VRF C_crypto = VRF Original.C_Crypto
  type DSIGN C_crypto = DSIGN.Ed25519DSIGN
  type HASH C_crypto = HASH Original.C_Crypto
  type ADDRHASH C_crypto = Hash.Blake2b_224
