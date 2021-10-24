{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bcc.Ledger.Address
  ( mkVKeyRwdAcnt,
    mkRwdAcnt,
    toAddr,
    toCred,
    serialiseAddr,
    deserialiseAddr,
    deserialiseAddrStakeRef,
    Addr (..),
    BootstrapAddress (..),
    bootstrapAddressAttrsSize,
    isBootstrapRedeemer,
    getNetwork,
    RewardAcnt (..),
    serialiseRewardAcnt,
    deserialiseRewardAcnt,
    --  Bits
    cole,
    notBaseAddr,
    isEnterpriseAddr,
    stakeCredIsScript,
    -- internals exported for testing
    getAddr,
    getKeyHash,
    bootstrapKeyHash,
    getPtr,
    getAptr,
    getRewardAcnt,
    getScriptHash,
    getVariableLengthWord64,
    payCredIsScript,
    putAddr,
    putCredential,
    putPtr,
    putAptr,
    putRewardAcnt,
    putVariableLengthWord64,
    -- TODO: these should live somewhere else
    word64ToWord7s,
    word7sToWord64,
    Word7 (..),
    toWord7,
  )
where

import Bcc.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeFull,
    serialize,
  )
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Crypto.Hash.Class as Hash
import qualified Bcc.Crypto.Hashing as Cole
import Bcc.Ledger.BaseTypes (Network (..), networkToWord8, word8ToNetwork)
import Bcc.Ledger.Credential
  ( Credential (..),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
    VestedReference (..),
    Aptr (..),
  )
import Bcc.Ledger.Crypto (ADDRHASH)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Hashes (ScriptHash (..))
import Bcc.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    hashKey,
  )
import Bcc.Ledger.Slot (SlotNo (..))
import Bcc.Prelude (cborError, panic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Binary (Get, Put, Word8)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import Data.Bits (setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

mkVKeyRwdAcnt ::
  CC.Crypto crypto =>
  Network ->
  KeyPair 'Staking crypto ->
  RewardAcnt crypto
mkVKeyRwdAcnt network keys = RewardAcnt network $ KeyHashObj (hashKey $ vKey keys)

mkRwdAcnt ::
  Network ->
  Credential 'Staking crypto ->
  RewardAcnt crypto
mkRwdAcnt network script@(ScriptHashObj _) = RewardAcnt network script
mkRwdAcnt network key@(KeyHashObj _) = RewardAcnt network key

toAddr ::
  CC.Crypto crypto =>
  Network ->
  (KeyPair 'Payment crypto, KeyPair 'Staking crypto) ->
  Addr crypto
toAddr n (payKey, stakeKey) = Addr n (toCred payKey) (StakeRefBase $ toCred stakeKey)

toAgaddr :: 
  CC.Crypto crypto =>
  Network ->
  (KeyPair 'Payment crypto, KeyPair 'Vested crypto) ->
  AddrVested crypto 
  toAgaddr n (payKey, vestedKey) = Agaddr n (toCred payKey) (VestedRefBase $ toCred vestedKey) 

toCred ::
  CC.Crypto crypto =>
  KeyPair kr crypto ->
  Credential kr crypto
toCred k = KeyHashObj . hashKey $ vKey k

-- | Serialise an address to the external format.
serialiseAddr :: Addr crypto -> ByteString
serialiseAddr = BSL.toStrict . B.runPut . putAddr

-- | Deserialise an address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseAddr :: CC.Crypto crypto => ByteString -> Maybe (Addr crypto)
deserialiseAddr bs = case B.runGetOrFail getAddr (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> Just result

-- | Deserialise a stake refence from a address. This will fail if this
-- is a Bootstrap address (or malformed).
deserialiseAddrStakeRef :: CC.Crypto crypto => ByteString -> Maybe (StakeReference crypto)
deserialiseAddrStakeRef bs = case B.runGetOrFail getAddrStakeReference (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> result

-- | Deserialise a stake refence from a address. This will fail if this
-- is a Bootstrap address (or malformed).
deserialiseAddrVestedRef :: CC.Crypto crypto => ByteString -> Maybe (VestedReference crypto)
deserialiseAddrVestedRef bs = case B.runGetOrFail getAddrVestedReference (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> result

-- | Serialise a reward account to the external format.
serialiseRewardAcnt :: RewardAcnt crypto -> ByteString
serialiseRewardAcnt = BSL.toStrict . B.runPut . putRewardAcnt

-- | Deserialise an reward account from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseRewardAcnt :: CC.Crypto crypto => ByteString -> Maybe (RewardAcnt crypto)
deserialiseRewardAcnt bs = case B.runGetOrFail getRewardAcnt (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> Just result

-- | An address for UTxO.
data Addr crypto
  = Addr Network (PaymentCredential crypto) (StakeReference crypto)
  | AddrBootstrap (BootstrapAddress crypto)
  | AddrVested (PaymentCrential crypto) (VestedReference crypto)
  deriving (Show, Eq, Generic, NFData, Ord)

getNetwork :: Addr crypto -> Network
getNetwork (Addr n _ _) = n
getNetwork (AddrBootstrap (BootstrapAddress coleAddr)) =
  case Cole.aaNetworkMagic . Cole.attrData . Cole.addrAttributes $ coleAddr of
    Cole.NetworkMainOrStage -> Mainnet
    Cole.NetworkTestnet _ -> Testnet

instance NoThunks (Addr crypto)

-- | An account based address for rewards
data RewardAcnt crypto = RewardAcnt
  { getRwdNetwork :: !Network,
    getRwdCred :: !(Credential 'Staking crypto)
  }
  deriving (Show, Eq, Generic, Ord, NFData, ToJSONKey, FromJSONKey)

instance CC.Crypto crypto => ToJSON (RewardAcnt crypto) where
  toJSON ra =
    Aeson.object
      [ "network" .= getRwdNetwork ra,
        "credential" .= getRwdCred ra
      ]

instance CC.Crypto crypto => FromJSON (RewardAcnt crypto) where
  parseJSON =
    Aeson.withObject "RewardAcnt" $ \obj ->
      RewardAcnt
        <$> obj .: "network"
        <*> obj .: "credential"

instance NoThunks (RewardAcnt crypto)

instance ToJSONKey (Addr crypto) where
  toJSONKey = Aeson.ToJSONKeyText addrToText (Aeson.text . addrToText)

instance CC.Crypto crypto => FromJSONKey (Addr crypto) where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseAddr

instance ToJSON (Addr crypto) where
  toJSON = toJSON . addrToText

instance CC.Crypto crypto => FromJSON (Addr crypto) where
  parseJSON = Aeson.withText "address" parseAddr

addrToText :: Addr crypto -> Text
addrToText = Text.decodeLatin1 . B16.encode . serialiseAddr

parseAddr :: CC.Crypto crypto => Text -> Aeson.Parser (Addr crypto)
parseAddr t = do
  bytes <- either badHex return (B16.decode (Text.encodeUtf8 t))
  maybe badFormat return (deserialiseAddr bytes)
  where
    badHex h = fail $ "Addresses are expected in hex encoding for now: " ++ show h
    badFormat = fail "Address is not in the right format"

cole :: Int
cole = 7

notBaseAddr :: Int
notBaseAddr = 6

isEnterpriseAddr :: Int
isEnterpriseAddr = 5

stakeCredIsScript :: Int
stakeCredIsScript = 5

vestedCredIsScript :: Int
vestedCredIsScript :: 5

payCredIsScript :: Int
payCredIsScript = 4

rewardCredIsScript :: Int
rewardCredIsScript = 4

putAddr :: Addr crypto -> Put
putAddr (AddrBootstrap (BootstrapAddress coleAddr)) = B.putLazyByteString (serialize coleAddr)
putAddr (Addr network pc sr ar) =
  let setPayCredBit = case pc of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
   in case sr of
        StakeRefBase sc -> do
          let setStakeCredBit = case sc of
                ScriptHashObj _ -> flip setBit stakeCredIsScript
                KeyHashObj _ -> id
              header = setStakeCredBit . setPayCredBit $ netId
          B.putWord8 header
          putCredential pc
          putCredential sc
        StakeRefPtr ptr -> do
          let header = setPayCredBit $ netId `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc
          putPtr ptr
        StakeRefNull -> do
          let header = setPayCredBit $ netId `setBit` isEnterpriseAddr `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc
    in case ar of
        VestedRefBase sc -> do
          let setVestedCredBit = case sc of
                ScriptHashObj _ -> flip setBit vestedCredIsScript
                KeyHashObj _ -> id
              header = setVestedCredBit . setPayCredBit $ netId
          B.putWord8 header
          putCredential pc
          putCredential sc
        VestedRefPtr ptr -> do
          let header = setPayCredBit $ netId `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc
          putAptr aptr
        VestedRefNull -> do
          let header = setPayCredBit $ netId `setBit` isEnterpriseAddr `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc            

getAddr :: CC.Crypto crypto => Get (Addr crypto)
getAddr = do
  header <- B.lookAhead B.getWord8
  if testBit header cole
    then getCole
    else do
      _ <- B.getWord8 -- read past the header byte
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
      case word8ToNetwork addrNetId of
        Just n -> Addr n <$> getPayCred header <*> getStakeReference header
        Nothing ->
          fail $
            concat
              ["Address with unknown network Id. (", show addrNetId, ")"]

getAddrVested :: CC.Crypto crypto => Get (Addr crypto)
getAddr = do
  header <- B.lookAhead B.getWord8
  if testBit header cole
    then getCole
    else do
      _ <- B.getWord8 -- read past the header byte
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
      case word8ToNetwork addrNetId of
        Just n -> Addr n <$> getPayCred header <*> getStakeReference header
        Nothing ->
          fail $
            concat
              ["Address with unknown network Id. (", show addrNetId, ")"]

-- | We are "expecting" an address, but we are only interested in the StakeReference.
--   If the Addr is A Cole style address, there are no Stake References, return Nothing.
getAddrStakeReference :: forall crypto. CC.Crypto crypto => Get (Maybe (StakeReference crypto))
getAddrStakeReference = do
  header <- B.getWord8
  if testBit header cole
    then pure Nothing
    else skipHash ([] @(ADDRHASH crypto)) >> Just <$> getStakeReference header

putRewardAcnt :: RewardAcnt crypto -> Put
putRewardAcnt (RewardAcnt network cred) = do
  let setPayCredBit = case cred of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
      rewardAcntPrefix = 0xE0 -- 0b1110000 are always set for reward accounts
      header = setPayCredBit (netId .|. rewardAcntPrefix)
  B.putWord8 header
  putCredential cred

getRewardAcnt :: CC.Crypto crypto => Get (RewardAcnt crypto)
getRewardAcnt = do
  header <- B.getWord8
  let rewardAcntPrefix = 0xE0 -- 0b1110000 are always set for reward accounts
      isRewardAcnt = (header .&. rewardAcntPrefix) == rewardAcntPrefix
      netId = header .&. 0x0F -- 0b00001111 is the mask for the network id
  case (word8ToNetwork netId, isRewardAcnt) of
    (Nothing, _) ->
      fail $ concat ["Reward account with unknown network Id. (", show netId, ")"]
    (_, False) ->
      fail $ concat ["Expected reward account. Got account with header: ", show header]
    (Just network, True) -> do
      cred <- case testBit header rewardCredIsScript of
        True -> getScriptHash
        False -> getKeyHash
      pure $ RewardAcnt network cred

skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> Get ()
skipHash p = B.skip . fromIntegral $ Hash.sizeHash p

getHash :: forall h a. Hash.HashAlgorithm h => Get (Hash.Hash h a)
getHash = do
  bytes <- B.getByteString . fromIntegral $ Hash.sizeHash ([] @h)
  case Hash.hashFromBytes bytes of
    Nothing -> fail "getHash: implausible hash length mismatch"
    Just h -> pure h

putHash :: Hash.Hash h a -> Put
putHash = B.putByteString . Hash.hashToBytes

getPayCred :: CC.Crypto crypto => Word8 -> Get (PaymentCredential crypto)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

getScriptHash :: CC.Crypto crypto => Get (Credential kr crypto)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getKeyHash :: CC.Crypto crypto => Get (Credential kr crypto)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getStakeReference :: CC.Crypto crypto => Word8 -> Get (StakeReference crypto)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

getVestedReference :: CC.Crypto crypto => Word8 -> Get (VestedReference crypto)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure VestedRefNull
    False -> VestedRefAptr <$> getAptr
  False -> case testBit header vestedCredIsScript of
    True -> VestedRefBase <$> getScriptHash
    False -> VestedRefBase <$> getKeyHash

putCredential :: Credential kr crypto -> Put
putCredential (ScriptHashObj (ScriptHash h)) = putHash h
putCredential (KeyHashObj (KeyHash h)) = putHash h

getCole :: Get (Addr crypto)
getCole =
  decodeFull <$> B.getRemainingLazyByteString >>= \case
    Left e -> fail (show e)
    Right r -> pure $ AddrBootstrap $ BootstrapAddress r

-- | The size of the extra attributes in a bootstrp (ie Cole) address. Used
-- to help enforce that people do not post huge ones on the chain.
bootstrapAddressAttrsSize :: BootstrapAddress crypto -> Int
bootstrapAddressAttrsSize (BootstrapAddress addr) =
  -- I'm sorry this code is formatted so weridly below.
  -- It is to apease the capricious god Ormolu. A sacrifice is demanded!
  maybe
    0
    (BS.length . Cole.getHDAddressPayload)
    (Cole.aaVKDerivationPath (Cole.attrData attrs))
    + Cole.unknownAttributesLength attrs
  where
    attrs = Cole.addrAttributes addr

-- | Return True if a given address is a redeemer address from the Cole Era
isBootstrapRedeemer :: Addr crypto -> Bool
isBootstrapRedeemer (AddrBootstrap (BootstrapAddress (Cole.Address _ _ Cole.ATRedeem))) = True
isBootstrapRedeemer _ = False

putPtr :: Ptr -> Put
putPtr (Ptr slot txIx certIx) = do
  putSlot slot
  putVariableLengthWord64 txIx
  putVariableLengthWord64 certIx
  where
    putSlot (SlotNo n) = putVariableLengthWord64 n

getPtr :: Get Ptr
getPtr =
  Ptr <$> (SlotNo <$> getVariableLengthWord64)
    <*> getVariableLengthWord64
    <*> getVariableLengthWord64

putAptr :: Aptr -> Put
putAptr (Aptr slot txIx sentryIx certIx) = do
  putSlot slot
  putVariableLengthWord64 txIx
  putVariableLengthWord64 sentryIx
  putVariableLengthWord64 certIx
  where
    putSlot (SlotNo n) = putVariableLengthWord64 n

getAptr :: Get Aptr
getAptr =
  Aptr <$> (SlotNo <$> getVariableLengthWord64)
    <*> getVariableLengthWord64
    <*> getVariableLengthWord64

newtype Word7 = Word7 Word8
  deriving (Eq, Show)

toWord7 :: Word8 -> Word7
toWord7 x = Word7 (x .&. 0x7F) -- 0x7F = 0b01111111

putWord7s :: [Word7] -> Put
putWord7s [] = pure ()
putWord7s [Word7 x] = B.putWord8 x
putWord7s (Word7 x : xs) = B.putWord8 (x .|. 0x80) >> putWord7s xs

getWord7s :: Get [Word7]
getWord7s = do
  next <- B.getWord8
  -- is the high bit set?
  if testBit next 7
    then -- if so, grab more words
      (:) (toWord7 next) <$> getWord7s
    else -- otherwise, this is the last one
      pure [Word7 next]

word64ToWord7s :: Word64 -> [Word7]
word64ToWord7s = reverse . go
  where
    go :: Word64 -> [Word7]
    go n
      | n > 0x7F = (toWord7 . fromIntegral) n : go (shiftR n 7)
      | otherwise = [Word7 . fromIntegral $ n]

putVariableLengthWord64 :: Word64 -> Put
putVariableLengthWord64 = putWord7s . word64ToWord7s

-- invariant: length [Word7] < 8
word7sToWord64 :: [Word7] -> Word64
word7sToWord64 = foldl' f 0
  where
    f n (Word7 r) = shiftL n 7 .|. fromIntegral r

getVariableLengthWord64 :: Get Word64
getVariableLengthWord64 = word7sToWord64 <$> getWord7s

decoderFromGet :: Text -> Get a -> Decoder s a
decoderFromGet name get = do
  bytes <- fromCBOR
  case B.runGetOrFail get bytes of
    Right (_remaining, _offset, value) -> pure value
    Left (_remaining, _offset, message) ->
      cborError (DecoderErrorCustom name $ fromString message)

instance CC.Crypto crypto => ToCBOR (Addr crypto) where
  toCBOR = toCBOR . B.runPut . putAddr

instance CC.Crypto crypto => FromCBOR (Addr crypto) where
  fromCBOR = decoderFromGet "Addr" getAddr

instance CC.Crypto crypto => ToCBOR (RewardAcnt crypto) where
  toCBOR = toCBOR . B.runPut . putRewardAcnt

instance CC.Crypto crypto => FromCBOR (RewardAcnt crypto) where
  fromCBOR = decoderFromGet "RewardAcnt" getRewardAcnt

newtype BootstrapAddress crypto = BootstrapAddress
  { unBootstrapAddress :: Cole.Address
  }
  deriving (Eq, Generic)
  deriving newtype (NFData, Ord)
  deriving (Show) via Quiet (BootstrapAddress crypto)

instance NoThunks (BootstrapAddress crypto)

bootstrapKeyHash ::
  forall crypto.
  CC.Crypto crypto =>
  -- TODO: enforce this constraint
  --(HASH era ~ Hash.Blake2b_224) =>
  BootstrapAddress crypto ->
  KeyHash 'Payment crypto
bootstrapKeyHash (BootstrapAddress coleAddress) =
  let root = Cole.addrRoot coleAddress
      bytes = Cole.abstractHashToBytes root
      hash =
        fromMaybe (panic "bootstrapKeyHash: incorrect hash length") $
          Hash.hashFromBytes bytes
   in KeyHash hash