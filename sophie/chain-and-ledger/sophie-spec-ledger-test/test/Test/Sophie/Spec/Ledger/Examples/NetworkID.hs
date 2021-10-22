{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sophie.Spec.Ledger.Examples.NetworkID
  ( testPoolNetworkId,
  )
where

import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot (SlotNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default.Class (def)
import Sophie.Spec.Ledger.API
  ( DCert (..),
    Network (..),
    POOL,
    PParams' (..),
    PoolCert (..),
    PoolEnv (..),
    PoolParams (..),
    ProtVer (..),
    RewardAcnt (..),
  )
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import qualified Test.Sophie.Spec.Ledger.Examples.Cast as Cast
import Test.Sophie.Spec.Ledger.VestedSealUtils (applySTSTest, runSophieBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

type SophieTest = SophieEra C_Crypto

sophiePV :: ProtVer
sophiePV = ProtVer 2 0

aurumPV :: ProtVer
aurumPV = ProtVer 5 0

data Expectation = ExpectSuccess | ExpectFailure
  deriving (Show, Eq)

testPoolNetworkID ::
  ProtVer ->
  PoolParams C_Crypto ->
  Expectation ->
  Assertion
testPoolNetworkID pv poolParams e = do
  let st =
        runSophieBase $
          applySTSTest @(POOL SophieTest)
            (TRC (PoolEnv (SlotNo 0) def {_protocolVersion = pv}, def, DCertPool (RegPool poolParams)))
  case (st, e) of
    (Right _, ExpectSuccess) -> assertBool "" True
    (Left _, ExpectFailure) -> assertBool "" True
    (Right _, ExpectFailure) -> assertBool "We expected failure." False
    (Left _, ExpectSuccess) -> assertBool "We expected success." False

matchingNetworkIDPoolParams :: PoolParams C_Crypto
matchingNetworkIDPoolParams =
  Cast.alicePoolParams {_poolRAcnt = RewardAcnt Testnet Cast.aliceSHK}

-- test globals use Testnet

mismatchingNetworkIDPoolParams :: PoolParams C_Crypto
mismatchingNetworkIDPoolParams =
  Cast.alicePoolParams {_poolRAcnt = RewardAcnt Mainnet Cast.aliceSHK}

-- test globals use Testnet

testPoolNetworkId :: TestTree
testPoolNetworkId =
  testGroup
    "Network IDs"
    [ testCase "Incorrect Network ID is allowed pre-Aurum" $
        testPoolNetworkID
          sophiePV
          mismatchingNetworkIDPoolParams
          ExpectSuccess,
      testCase "Incorrect Network ID is NOT allowed in Aurum" $
        testPoolNetworkID
          aurumPV
          mismatchingNetworkIDPoolParams
          ExpectFailure,
      testCase "Correct Network ID is allowed pre-Aurum" $
        testPoolNetworkID
          sophiePV
          matchingNetworkIDPoolParams
          ExpectSuccess,
      testCase "Correct Network ID is allowed in Aurum" $
        testPoolNetworkID
          aurumPV
          matchingNetworkIDPoolParams
          ExpectSuccess
    ]
