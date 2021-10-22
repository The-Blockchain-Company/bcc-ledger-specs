{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Bcc.Ledger.Jen.Golden
-- Description : Golden Tests for the Jen era
module Test.Bcc.Ledger.Aurum.Golden
  ( goldenUTxOEntryMinBcc,
  )
where

import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (Data (..), hashData)
import Bcc.Ledger.Aurum.Rules.Utxo (utxoEntrySize)
import Bcc.Ledger.Aurum.TxBody (TxOut (..))
import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import Bcc.Ledger.Coin (Coin (..))
import Bcc.Ledger.Jen.Value (Value (..), valueFromList)
import Data.Char (chr)
import Zerepoch.V1.Ledger.Api (Data (..))
import Test.Bcc.Ledger.EraBuffet (StandardCrypto)
import Test.Bcc.Ledger.Jen.Golden
  ( largestName,
    minUTxO,
    pid1,
    pid2,
    pid3,
    smallName,
    smallestName,
  )
import Test.Sophie.Spec.Ledger.Examples.Cast (aliceAddr, bobAddr, carlAddr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | bcc cost of storing a word8 of data as a UTxO entry, assuming no change to minUTxOValue
coinsPerUTxOWordLocal :: Integer
coinsPerUTxOWordLocal = quot minUTxOValueSophieMA utxoEntrySizeWithoutValLocal
  where
    utxoEntrySizeWithoutValLocal = 29
    Coin minUTxOValueSophieMA = minUTxO

calcMinUTxO :: TxOut (AurumEra StandardCrypto) -> Coin
calcMinUTxO tout = Coin (utxoEntrySize tout * coinsPerUTxOWordLocal)

-- | (heapWords of a DataHash) * coinsPerUTxOWordLocal is 344820
goldenUTxOEntryMinBcc :: TestTree
goldenUTxOEntryMinBcc =
  testGroup
    "golden tests - UTxOEntryMinBcc"
    [ testCase "one policy, one (smallest) name, yes datum hash" $
        calcMinUTxO
          ( TxOut
              carlAddr
              (valueFromList 1407406 [(pid1, smallestName, 1)])
              (SJust $ hashData @(AurumEra StandardCrypto) (Data (List [])))
          )
          @?= Coin 1655136,
      testCase "one policy, one (smallest) name, no datum hash" $
        calcMinUTxO
          ( TxOut
              bobAddr
              (valueFromList 1407406 [(pid1, smallestName, 1)])
              SNothing
          )
          @?= Coin 1310316,
      testCase "one policy, one (small) name" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1444443 [(pid1, smallName '1', 1)])
              SNothing
          )
          @?= Coin 1344798,
      testCase "one policy, three (small) names" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1555554 [(pid1, smallName '1', 1), (pid1, smallName '2', 1), (pid1, smallName '3', 1)])
              SNothing
          )
          @?= Coin 1448244,
      testCase "one policy, one (largest) name" $
        calcMinUTxO
          ( TxOut
              carlAddr
              (valueFromList 1555554 [(pid1, largestName 'a', 1)])
              SNothing
          )
          @?= Coin 1448244,
      testCase "one policy, three (largest) name, with hash" $
        calcMinUTxO
          ( TxOut
              carlAddr
              ( valueFromList
                  1962961
                  [ (pid1, largestName 'a', 1),
                    (pid1, largestName 'b', 1),
                    (pid1, largestName 'c', 1)
                  ]
              )
              (SJust $ hashData @(AurumEra StandardCrypto) (Data (Constr 0 [(Constr 0 [])])))
          )
          @?= Coin 2172366,
      testCase "two policies, one (smallest) name" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1592591 [(pid1, smallestName, 1), (pid2, smallestName, 1)])
              SNothing
          )
          @?= Coin 1482726,
      testCase "two policies, one (smallest) name, with hash" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              (valueFromList 1592591 [(pid1, smallestName, 1), (pid2, smallestName, 1)])
              (SJust $ hashData @(AurumEra StandardCrypto) (Data (Constr 0 [])))
          )
          @?= Coin 1827546,
      testCase "two policies, two (small) names" $
        calcMinUTxO
          ( TxOut
              bobAddr
              (valueFromList 1629628 [(pid1, smallName '1', 1), (pid2, smallName '2', 1)])
              SNothing
          )
          @?= Coin 1517208,
      testCase "three policies, ninety-six (small) names" $
        calcMinUTxO
          ( TxOut
              aliceAddr
              ( let f i c = (i, smallName (chr c), 1)
                 in valueFromList 7407400 [f i c | (i, cs) <- [(pid1, [32 .. 63]), (pid2, [64 .. 95]), (pid3, [96 .. 127])], c <- cs]
              )
              {-
                            ( Value 7407400 $
                                Map.fromList
                                  [ ( pid1,
                                      (Map.fromList $ map ((,1) . smallName . chr) [32 .. 63])
                                    ),
                                    ( pid2,
                                      (Map.fromList $ map ((,1) . smallName . chr) [64 .. 95])
                                    ),
                                    ( pid3,
                                      (Map.fromList $ map ((,1) . smallName . chr) [96 .. 127])
                                    )
                                  ]
                            )
              -}
              SNothing
          )
          @?= Coin 6896400,
      testCase "utxo entry size of bcc-only" $
        -- This value, 29, is helpful for comparing the aurum protocol parameter utxoCostPerWord
        -- with the old parameter minUTxOValue.
        -- If we wish to keep the bcc-only, no datum hash, minimum value nearly the same,
        -- we can divide minUTxOValue by 29 and round.
        utxoEntrySize @(AurumEra StandardCrypto) (TxOut aliceAddr (Value 0 mempty) SNothing) @?= 29
    ]
