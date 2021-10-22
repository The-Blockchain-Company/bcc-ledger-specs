{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Bcc.Ledger.Jen.Examples.MultiAssets
-- Description : Multi-Assets Examples
--
-- Examples demonstrating the use of multi-assets.
module Test.Bcc.Ledger.Jen.Examples.MultiAssets
  ( multiAssetsExample,
  )
where

import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import Bcc.Ledger.Coin (Coin (..))
import Bcc.Ledger.Keys (KeyPair (..), asWitness, hashKey)
import Bcc.Ledger.Jen (JenEra)
import Bcc.Ledger.Jen.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Ledger.SophieMA.Rules.Utxo (UtxoPredicateFailure (..))
import Bcc.Ledger.SophieMA.Timelocks (Timelock (..), ValidityInterval (..))
import Bcc.Ledger.SophieMA.TxBody (TxBody (..))
import Bcc.Ledger.Slot (SlotNo (..))
import Bcc.Ledger.Val ((<+>), (<->))
import qualified Bcc.Ledger.Val as Val
import Control.Exception (ErrorCall (ErrorCall), evaluate, try)
import Control.State.Transition.Extended (PredicateFailure)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Sophie.Spec.Ledger.API (LEDGER, LedgerEnv (..))
import Sophie.Spec.Ledger.LedgerState (AccountState (..))
import Sophie.Spec.Ledger.PParams (PParams, PParams' (..), emptyPParams)
import Sophie.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (..))
import Sophie.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
    hashScript,
  )
import Sophie.Spec.Ledger.TxBody
  ( TxId,
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Sophie.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import Test.Bcc.Ledger.EraBuffet (TestCrypto)
import Test.Bcc.Ledger.Jen.Examples (testJenNoDelegLEDGER)
import qualified Test.Bcc.Ledger.Jen.Examples.Cast as Cast
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

type JenTest = JenEra TestCrypto

------------------------------
-- Set Up the Initial State --
------------------------------

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

unboundedInterval :: ValidityInterval
unboundedInterval = ValidityInterval SNothing SNothing

bootstrapTxId :: TxId TestCrypto
bootstrapTxId = txid txb
  where
    txb :: TxBody JenTest
    txb =
      TxBody
        Set.empty
        StrictSeq.empty
        StrictSeq.empty
        (Wdrl Map.empty)
        (Coin 0)
        unboundedInterval
        SNothing
        SNothing
        (Val.inject (Coin 0))

initUTxO :: UTxO JenTest
initUTxO =
  UTxO $
    Map.fromList
      [ (TxIn bootstrapTxId 0, TxOut Cast.aliceAddr (Val.inject aliceInitCoin)),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

pp :: PParams JenTest
pp =
  emptyPParams
    { _minfeeA = 0,
      _minfeeB = 1,
      _maxTxSize = 16384,
      _minUTxOValue = Coin 100
    }

ledgerEnv :: SlotNo -> LedgerEnv JenTest
ledgerEnv s = LedgerEnv s 0 pp (AccountState (Coin 0) (Coin 0))

feeEx :: Coin
feeEx = Coin 3

-- These examples do not use several of the transaction components,
-- so we can simplify building them.
makeTxb ::
  [TxIn TestCrypto] ->
  [TxOut JenTest] ->
  ValidityInterval ->
  Value TestCrypto ->
  TxBody JenTest
makeTxb ins outs interval minted =
  TxBody
    (Set.fromList ins)
    (StrictSeq.fromList outs)
    StrictSeq.empty
    (Wdrl Map.empty)
    feeEx
    interval
    SNothing
    SNothing
    minted

policyFailure :: PolicyID TestCrypto -> Either [PredicateFailure (LEDGER JenTest)] (UTxO JenTest)
policyFailure p =
  Left
    [ UtxowFailure
        (ScriptWitnessNotValidatingUTXOW (Set.singleton (policyID p)))
    ]

outTooBigFailure :: TxOut JenTest -> Either [PredicateFailure (LEDGER JenTest)] (UTxO JenTest)
outTooBigFailure out = Left [UtxowFailure (UtxoFailure (OutputTooBigUTxO [out]))]

----------------------------------------------------
-- Introduce a new Token Bundle, Purple Tokens
--
-- Variables ending with SimpleExN (for a numeral N)
-- refer to this example.
----------------------------------------------------

-- This is the most lax policy possible, requiring no authorization at all.
purplePolicy :: Timelock TestCrypto
purplePolicy = RequireAllOf (StrictSeq.fromList [])

purplePolicyId :: PolicyID TestCrypto
purplePolicyId = PolicyID $ hashScript @JenTest purplePolicy

plum :: AssetName
plum = AssetName $ BS.pack "plum"

amethyst :: AssetName
amethyst = AssetName $ BS.pack "amethyst"

------------------------
-- Mint Purple Tokens --
------------------------

mintSimpleEx1 :: Value TestCrypto
mintSimpleEx1 =
  Value 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

aliceCoinSimpleEx1 :: Coin
aliceCoinSimpleEx1 = aliceInitCoin <-> feeEx

tokensSimpleEx1 :: Value TestCrypto
tokensSimpleEx1 = mintSimpleEx1 <+> Val.inject aliceCoinSimpleEx1

-- Mint a purple token bundle, consisting of thirteen plums and two amethysts.
-- Give the bundle to Alice.
txbodySimpleEx1 :: TxBody JenTest
txbodySimpleEx1 =
  makeTxb
    [TxIn bootstrapTxId 0]
    [TxOut Cast.aliceAddr tokensSimpleEx1]
    unboundedInterval
    mintSimpleEx1

txSimpleEx1 :: Tx JenTest
txSimpleEx1 =
  Tx
    txbodySimpleEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodySimpleEx1) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
      }
    SNothing

expectedUTxOSimpleEx1 :: UTxO JenTest
expectedUTxOSimpleEx1 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodySimpleEx1) 0, TxOut Cast.aliceAddr tokensSimpleEx1),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------
-- Transfer Purple Tokens --
----------------------------

minUtxoSimpleEx2 :: Coin
minUtxoSimpleEx2 = Coin 115

aliceCoinsSimpleEx2 :: Coin
aliceCoinsSimpleEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> minUtxoSimpleEx2)

aliceTokensSimpleEx2 :: Value TestCrypto
aliceTokensSimpleEx2 =
  Value (unCoin aliceCoinsSimpleEx2) $
    Map.singleton purplePolicyId (Map.fromList [(plum, 8), (amethyst, 2)])

bobTokensSimpleEx2 :: Value TestCrypto
bobTokensSimpleEx2 =
  Value (unCoin minUtxoSimpleEx2) $
    Map.singleton purplePolicyId (Map.singleton plum 5)

-- Alice gives five plums to Bob.
txbodySimpleEx2 :: TxBody JenTest
txbodySimpleEx2 =
  makeTxb
    [TxIn (txid txbodySimpleEx1) 0]
    [ TxOut Cast.aliceAddr aliceTokensSimpleEx2,
      TxOut Cast.bobAddr bobTokensSimpleEx2
    ]
    unboundedInterval
    Val.zero

txSimpleEx2 :: Tx JenTest
txSimpleEx2 =
  Tx
    txbodySimpleEx2
    mempty {addrWits = makeWitnessesVKey (hashAnnotated txbodySimpleEx2) [asWitness Cast.alicePay]}
    SNothing

expectedUTxOSimpleEx2 :: UTxO JenTest
expectedUTxOSimpleEx2 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodySimpleEx2) 0, TxOut Cast.aliceAddr aliceTokensSimpleEx2),
        (TxIn (txid txbodySimpleEx2) 1, TxOut Cast.bobAddr bobTokensSimpleEx2),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens With a Time Range
--
-- Variables ending with TimeExN (for a numeral N)
-- refer to this example.
------------------------------------------------------------

beforeStart :: SlotNo
beforeStart = SlotNo 12

startInterval :: SlotNo
startInterval = SlotNo 13

stopInterval :: SlotNo
stopInterval = SlotNo 19

afterStop :: SlotNo
afterStop = SlotNo 20

boundedTimePolicy :: Timelock TestCrypto
boundedTimePolicy =
  RequireAllOf
    ( StrictSeq.fromList
        [ RequireTimeStart startInterval,
          RequireTimeExpire stopInterval
        ]
    )

boundedTimePolicyId :: PolicyID TestCrypto
boundedTimePolicyId = PolicyID $ hashScript @JenTest boundedTimePolicy

tokenTimeEx :: AssetName
tokenTimeEx = AssetName $ BS.pack "tokenTimeEx"

------------------------------------
-- Mint Bounded Time Range Tokens --
------------------------------------

mintTimeEx1 :: Value TestCrypto
mintTimeEx1 =
  Value 0 $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx1 :: Coin
aliceCoinsTimeEx1 = aliceInitCoin <-> feeEx

tokensTimeEx1 :: Value TestCrypto
tokensTimeEx1 = mintTimeEx1 <+> Val.inject aliceCoinsTimeEx1

-- Mint tokens
txbodyTimeEx1 :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> TxBody JenTest
txbodyTimeEx1 s e =
  makeTxb
    [TxIn bootstrapTxId 0]
    [TxOut Cast.aliceAddr tokensTimeEx1]
    (ValidityInterval s e)
    mintTimeEx1

txbodyTimeEx1Valid :: TxBody JenTest
txbodyTimeEx1Valid = txbodyTimeEx1 (SJust startInterval) (SJust stopInterval)

txTimeEx1 :: TxBody JenTest -> Tx JenTest
txTimeEx1 txbody =
  Tx
    txbody
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbody) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID boundedTimePolicyId, boundedTimePolicy)]
      }
    SNothing

txTimeEx1Valid :: Tx JenTest
txTimeEx1Valid = txTimeEx1 txbodyTimeEx1Valid

txTimeEx1InvalidLHSfixed :: Tx JenTest
txTimeEx1InvalidLHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust beforeStart) (SJust stopInterval)

txTimeEx1InvalidLHSopen :: Tx JenTest
txTimeEx1InvalidLHSopen = txTimeEx1 $ txbodyTimeEx1 SNothing (SJust stopInterval)

txTimeEx1InvalidRHSfixed :: Tx JenTest
txTimeEx1InvalidRHSfixed = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) (SJust afterStop)

txTimeEx1InvalidRHSopen :: Tx JenTest
txTimeEx1InvalidRHSopen = txTimeEx1 $ txbodyTimeEx1 (SJust startInterval) SNothing

expectedUTxOTimeEx1 :: UTxO JenTest
expectedUTxOTimeEx1 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyTimeEx1Valid) 0, TxOut Cast.aliceAddr tokensTimeEx1),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

----------------------------------------
-- Transfer Bounded Time Range Tokens --
----------------------------------------

mintTimeEx2 :: Coin
mintTimeEx2 = Coin 120

bobTokensTimeEx2 :: Value TestCrypto
bobTokensTimeEx2 =
  Value (unCoin mintTimeEx2) $
    Map.singleton boundedTimePolicyId (Map.singleton tokenTimeEx 1)

aliceCoinsTimeEx2 :: Coin
aliceCoinsTimeEx2 = aliceCoinSimpleEx1 <-> (feeEx <+> mintTimeEx2)

-- Alice gives one token to Bob
txbodyTimeEx2 :: TxBody JenTest
txbodyTimeEx2 =
  makeTxb
    [TxIn (txid txbodyTimeEx1Valid) 0]
    [ TxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2),
      TxOut Cast.bobAddr bobTokensTimeEx2
    ]
    unboundedInterval
    Val.zero

txTimeEx2 :: Tx JenTest
txTimeEx2 =
  Tx
    txbodyTimeEx2
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated txbodyTimeEx2) [asWitness Cast.alicePay]
      }
    SNothing

expectedUTxOTimeEx2 :: UTxO JenTest
expectedUTxOTimeEx2 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyTimeEx2) 0, TxOut Cast.aliceAddr (Val.inject aliceCoinsTimeEx2)),
        (TxIn (txid txbodyTimeEx2) 1, TxOut Cast.bobAddr bobTokensTimeEx2),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin))
      ]

--------------------------------------------------------------
-- Introduce a new Token Bundle, Tokens only Alice can mint
--
-- Variables ending with SingExN (for a numeral N)
-- refer to this example.
--------------------------------------------------------------

alicePolicy :: Timelock TestCrypto
alicePolicy = RequireSignature . asWitness . hashKey . vKey $ Cast.alicePay

alicePolicyId :: PolicyID TestCrypto
alicePolicyId = PolicyID $ hashScript @JenTest alicePolicy

tokenSingWitEx1 :: AssetName
tokenSingWitEx1 = AssetName $ BS.pack "tokenSingWitEx1"

-----------------------
-- Mint Alice Tokens --
-----------------------

mintSingWitEx1 :: Value TestCrypto
mintSingWitEx1 =
  Value 0 $
    Map.singleton alicePolicyId (Map.singleton tokenSingWitEx1 17)

bobCoinsSingWitEx1 :: Coin
bobCoinsSingWitEx1 = bobInitCoin <-> feeEx

tokensSingWitEx1 :: Value TestCrypto
tokensSingWitEx1 = mintSingWitEx1 <+> Val.inject bobCoinsSingWitEx1

-- Bob pays the fees, but only alice can witness the minting
txbodySingWitEx1 :: TxBody JenTest
txbodySingWitEx1 =
  makeTxb
    [TxIn bootstrapTxId 1]
    [TxOut Cast.bobAddr tokensSingWitEx1]
    unboundedInterval
    mintSingWitEx1

txSingWitEx1Valid :: Tx JenTest
txSingWitEx1Valid =
  Tx
    txbodySingWitEx1
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated txbodySingWitEx1) [asWitness Cast.bobPay, asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID alicePolicyId, alicePolicy)]
      }
    SNothing

expectedUTxOSingWitEx1 :: UTxO JenTest
expectedUTxOSingWitEx1 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodySingWitEx1) 0, TxOut Cast.bobAddr tokensSingWitEx1),
        (TxIn bootstrapTxId 0, TxOut Cast.aliceAddr (Val.inject aliceInitCoin))
      ]

txSingWitEx1Invalid :: Tx JenTest
txSingWitEx1Invalid =
  Tx
    txbodySingWitEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodySingWitEx1) [asWitness Cast.bobPay],
        scriptWits = Map.fromList [(policyID alicePolicyId, alicePolicy)]
      }
    SNothing

------------------------
-- Mint Negative Values
--
-- Variables ending with NegExN (for a numeral N)
-- refer to this example. We assume that the simple
-- tokens in the SimpleEx1 example have been minted
-- and we use expectedUTxOSimpleEx1 as our starting
-- state.
------------------------

-- Mint negative valued tokens
mintNegEx1 :: Value TestCrypto
mintNegEx1 =
  Value 0 $
    Map.singleton purplePolicyId (Map.singleton plum (-8))

aliceTokensNegEx1 :: Value TestCrypto
aliceTokensNegEx1 =
  Value (unCoin $ aliceCoinsSimpleEx2 <-> feeEx) $
    Map.singleton purplePolicyId (Map.singleton amethyst 2)

txbodyNegEx1 :: TxBody JenTest
txbodyNegEx1 =
  makeTxb
    [TxIn (txid txbodySimpleEx2) 0]
    [TxOut Cast.aliceAddr aliceTokensNegEx1]
    unboundedInterval
    mintNegEx1

txNegEx1 :: Tx JenTest
txNegEx1 =
  Tx
    txbodyNegEx1
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodyNegEx1) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
      }
    SNothing

initialUTxONegEx1 :: UTxO JenTest
initialUTxONegEx1 = expectedUTxOSimpleEx2

expectedUTxONegEx1 :: UTxO JenTest
expectedUTxONegEx1 =
  UTxO $
    Map.fromList
      [ (TxIn (txid txbodyNegEx1) 0, TxOut Cast.aliceAddr aliceTokensNegEx1),
        (TxIn bootstrapTxId 1, TxOut Cast.bobAddr (Val.inject bobInitCoin)),
        (TxIn (txid txbodySimpleEx2) 1, TxOut Cast.bobAddr bobTokensSimpleEx2)
      ]

--
-- Now attempt to produce negative outputs
--

mintNegEx2 :: Value TestCrypto
mintNegEx2 =
  Value 0 $
    Map.singleton purplePolicyId (Map.singleton plum (-9))

aliceTokensNegEx2 :: Value TestCrypto
aliceTokensNegEx2 =
  Value (unCoin $ aliceCoinsSimpleEx2 <-> feeEx) $
    Map.singleton purplePolicyId (Map.fromList [(plum, -1), (amethyst, 2)])

-- Mint negative valued tokens
txbodyNegEx2 :: TxBody JenTest
txbodyNegEx2 =
  makeTxb
    [TxIn (txid txbodySimpleEx2) 0]
    [TxOut Cast.aliceAddr aliceTokensNegEx2]
    unboundedInterval
    mintNegEx2

testNegEx2 :: Assertion
testNegEx2 = do
  r <- try (evaluate $ txbodyNegEx2 == txbodyNegEx2)
  case r of
    Left (ErrorCall _) -> pure ()
    Right _ -> assertFailure "constructed negative TxOut Value"

--
-- Create a Value that is too big
--

minUtxoBigEx :: Coin
minUtxoBigEx = Coin 50000

smallValue :: Value TestCrypto
smallValue =
  Value 0 $
    Map.singleton purplePolicyId (Map.fromList [(plum, 13), (amethyst, 2)])

smallOut :: TxOut JenTest
smallOut =
  TxOut Cast.aliceAddr $ smallValue <+> Val.inject (aliceInitCoin <-> (feeEx <+> minUtxoBigEx))

numAssets :: Int
numAssets = 1000

bigValue :: Value TestCrypto
bigValue =
  Value 0 $
    Map.singleton
      purplePolicyId
      (Map.fromList $ map (\x -> (AssetName . BS.pack $ show x, 1)) [1 .. numAssets])

bigOut :: TxOut JenTest
bigOut = TxOut Cast.aliceAddr $ bigValue <+> Val.inject minUtxoBigEx

txbodyWithBigValue :: TxBody JenTest
txbodyWithBigValue =
  makeTxb
    [TxIn bootstrapTxId 0]
    [smallOut, bigOut]
    unboundedInterval
    (bigValue <+> smallValue)

txBigValue :: Tx JenTest
txBigValue =
  Tx
    txbodyWithBigValue
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbodyWithBigValue) [asWitness Cast.alicePay],
        scriptWits = Map.fromList [(policyID purplePolicyId, purplePolicy)]
      }
    SNothing

--
-- Multi-Assets Test Group
--

multiAssetsExample :: TestTree
multiAssetsExample =
  testGroup
    "multi-assets"
    [ testGroup
        "simple"
        [ testCase "minting" $
            testJenNoDelegLEDGER
              initUTxO
              txSimpleEx1
              (ledgerEnv $ SlotNo 0)
              (Right expectedUTxOSimpleEx1),
          testCase "transfer" $
            testJenNoDelegLEDGER
              expectedUTxOSimpleEx1
              txSimpleEx2
              (ledgerEnv $ SlotNo 1)
              (Right expectedUTxOSimpleEx2)
        ],
      testGroup
        "bounded time interval"
        [ testCase "minting, valid" $
            testJenNoDelegLEDGER
              initUTxO
              txTimeEx1Valid
              (ledgerEnv startInterval)
              (Right expectedUTxOTimeEx1),
          testCase "minting, invalid LHS too small" $
            testJenNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidLHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid LHS unspecified" $
            testJenNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidLHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid RHS too big" $
            testJenNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidRHSfixed
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "minting, invalid RHS unspecified" $
            testJenNoDelegLEDGER
              initUTxO
              txTimeEx1InvalidRHSopen
              (ledgerEnv startInterval)
              (policyFailure boundedTimePolicyId),
          testCase "transfer, after minting period" $
            testJenNoDelegLEDGER
              expectedUTxOTimeEx1
              txTimeEx2
              (ledgerEnv afterStop)
              (Right expectedUTxOTimeEx2)
        ],
      testGroup
        "single key"
        [ testCase "minting, valid" $
            testJenNoDelegLEDGER
              initUTxO
              txSingWitEx1Valid
              (ledgerEnv $ SlotNo 0)
              (Right expectedUTxOSingWitEx1),
          testCase "minting, invalid no mint signature" $
            testJenNoDelegLEDGER
              initUTxO
              txSingWitEx1Invalid
              (ledgerEnv $ SlotNo 0)
              (policyFailure alicePolicyId)
        ],
      testGroup
        "negative minting"
        [ testCase "remove assets" $
            testJenNoDelegLEDGER
              initialUTxONegEx1
              txNegEx1
              (ledgerEnv $ SlotNo 3)
              (Right expectedUTxONegEx1),
          testCase "no negative outputs" testNegEx2
        ],
      testCase "value too big" $
        testJenNoDelegLEDGER
          initUTxO
          txBigValue
          (ledgerEnv $ SlotNo 0)
          (outTooBigFailure bigOut)
    ]
