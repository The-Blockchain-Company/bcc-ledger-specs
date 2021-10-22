{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Bcc.Chain.Common.Gen
  ( genAddrAttributes,
    genAddrAttributesWithNM,
    genAddress,
    genAddressWithNM,
    genAddrType,
    genAddrSpendingData,
    genAttributes,
    genBlockCount,
    genCanonicalTxFeePolicy,
    genChainDifficulty,
    genCompactAddress,
    genCustomEntropic,
    genEntropic,
    genEntropicError,
    genEntropicWithRange,
    genEntropicPortion,
    genMerkleRoot,
    genMerkleTree,
    genNetworkMagic,
    genScriptVersion,
    genKeyHash,
    genTxFeePolicy,
    genTxSizeLinear,
  )
where

import Bcc.Binary (ToCBOR)
import Bcc.Chain.Common
  ( AddrAttributes (..),
    AddrSpendingData (..),
    AddrType (..),
    Address (..),
    Attributes,
    BlockCount (..),
    ChainDifficulty (..),
    CompactAddress,
    HDAddressPayload (..),
    KeyHash,
    Entropic,
    EntropicError (..),
    EntropicPortion,
    MerkleRoot (..),
    MerkleTree,
    NetworkMagic (..),
    TxFeePolicy (..),
    TxSizeLinear (..),
    hashKey,
    makeAddress,
    maxEntropicVal,
    mkAttributes,
    mkEntropic,
    mkMerkleTree,
    mtRoot,
    rationalToEntropicPortion,
    toCompactAddress,
  )
import Bcc.Prelude
import Formatting (build, sformat)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Bcc.Crypto.Gen (genRedeemVerificationKey, genVerificationKey)
import Test.Bcc.Prelude (gen32Bytes)

genAddrAttributes :: Gen AddrAttributes
genAddrAttributes = genAddrAttributesWithNM =<< genNetworkMagic

genAddrAttributesWithNM :: NetworkMagic -> Gen AddrAttributes
genAddrAttributesWithNM nm = AddrAttributes <$> hap <*> pure nm
  where
    hap = Gen.maybe genHDAddressPayload

genHDAddressPayload :: Gen HDAddressPayload
genHDAddressPayload = HDAddressPayload <$> gen32Bytes

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddressWithNM :: NetworkMagic -> Gen Address
genAddressWithNM nm =
  makeAddress <$> genAddrSpendingData
    <*> genAddrAttributesWithNM nm

genAddrType :: Gen AddrType
genAddrType = Gen.choice [pure ATVerKey, pure ATRedeem]

genAddrSpendingData :: Gen AddrSpendingData
genAddrSpendingData =
  Gen.choice [VerKeyASD <$> genVerificationKey, RedeemASD <$> genRedeemVerificationKey]

genAttributes :: Gen a -> Gen (Attributes a)
genAttributes genA = mkAttributes <$> genA

genBlockCount :: Gen BlockCount
genBlockCount = BlockCount <$> Gen.word64 Range.constantBounded

genCanonicalTxFeePolicy :: Gen TxFeePolicy
genCanonicalTxFeePolicy = TxFeePolicyTxSizeLinear <$> genCanonicalTxSizeLinear

genCanonicalTxSizeLinear :: Gen TxSizeLinear
genCanonicalTxSizeLinear = TxSizeLinear <$> genEntropic' <*> genMultiplier
  where
    genEntropic' :: Gen Entropic
    genEntropic' =
      mkEntropic
        <$> Gen.word64 (Range.constant 0 maxCanonicalEntropicVal)
        >>= \case
          Right entropic -> pure entropic
          Left err ->
            panic $
              sformat
                ("The impossible happened in genEntropic: " . build)
                err

    maxCanonicalEntropicVal :: Word64
    maxCanonicalEntropicVal = 9e6

genChainDifficulty :: Gen ChainDifficulty
genChainDifficulty = ChainDifficulty <$> Gen.word64 Range.constantBounded

genCompactAddress :: Gen CompactAddress
genCompactAddress = toCompactAddress <$> genAddress

genCustomEntropic :: Word64 -> Gen Entropic
genCustomEntropic size = genEntropicWithRange (Range.linear 0 size)

genEntropic :: Gen Entropic
genEntropic = genEntropicWithRange (Range.constant 0 maxEntropicVal)

genEntropicError :: Gen EntropicError
genEntropicError =
  Gen.choice
    [ EntropicOverflow <$> Gen.word64 overflowRange,
      EntropicTooLarge <$> Gen.integral tooLargeRange,
      EntropicTooSmall <$> Gen.integral tooSmallRange,
      uncurry EntropicUnderflow <$> genUnderflowErrorValues
    ]
  where
    overflowRange :: Range Word64
    overflowRange = Range.constant (maxEntropicVal + 1) (maxBound :: Word64)

    tooLargeRange :: Range Integer
    tooLargeRange =
      Range.constant
        (fromIntegral (maxEntropicVal + 1))
        (fromIntegral (maxBound :: Word64))

    tooSmallRange :: Range Integer
    tooSmallRange = Range.constant (fromIntegral (minBound :: Int)) (- 1)

    genUnderflowErrorValues :: Gen (Word64, Word64)
    genUnderflowErrorValues = do
      a <- Gen.word64 (Range.constant 0 (maxBound - 1))
      b <- Gen.word64 (Range.constant a maxBound)
      pure (a, b)

genEntropicWithRange :: Range Word64 -> Gen Entropic
genEntropicWithRange r =
  mkEntropic <$> Gen.word64 r >>= \case
    Right entropic -> pure entropic
    Left err ->
      panic $ sformat ("The impossible happened in genEntropic: " . build) err

genEntropicPortion :: Gen EntropicPortion
genEntropicPortion =
  rationalToEntropicPortion . realToFrac <$> Gen.double (Range.constant 0 1)

-- slow
genMerkleTree :: ToCBOR a => Gen a -> Gen (MerkleTree a)
genMerkleTree genA = mkMerkleTree <$> Gen.list (Range.linear 0 10) genA

-- slow
genMerkleRoot :: ToCBOR a => Gen a -> Gen (MerkleRoot a)
genMerkleRoot genA = mtRoot <$> genMerkleTree genA

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic =
  Gen.choice
    [ pure NetworkMainOrStage,
      NetworkTestnet <$> Gen.word32 Range.constantBounded
    ]

genScriptVersion :: Gen Word16
genScriptVersion = Gen.word16 Range.constantBounded

genKeyHash :: Gen KeyHash
genKeyHash = hashKey <$> genVerificationKey

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy = TxFeePolicyTxSizeLinear <$> genTxSizeLinear

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genEntropic <*> genMultiplier

-- | Generate multipliers for the TxSizeLinear.
genMultiplier :: Gen Rational
genMultiplier = fromIntegral <$> Gen.word16 (Range.constant 0 1000)
