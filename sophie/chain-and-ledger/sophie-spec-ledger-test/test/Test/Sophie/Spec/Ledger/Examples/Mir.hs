{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Sophie.Spec.Ledger.Examples.Mir
-- Description : MIR Example
--
-- Example demonstrating the Move Instantaneous Rewards mechanism
module Test.Sophie.Spec.Ledger.Examples.Mir
  ( mirExample,
  )
where

import Bcc.Ledger.BaseTypes (Nonce, StrictMaybe (..))
import Bcc.Ledger.Coin (Coin (..), toDeltaCoin)
import Bcc.Ledger.Credential (Ptr (..))
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (Crypto (..))
import Bcc.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
    asWitness,
  )
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot (BlockNo (..), SlotNo (..))
import Bcc.Ledger.Val ((<+>), (<->))
import qualified Bcc.Ledger.Val as Val
import Bcc.Protocol.TOptimum.BHeader (bhHash)
import Bcc.Protocol.TOptimum.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Sophie.Spec.Ledger.BlockChain (Block, bheader)
import Sophie.Spec.Ledger.Delegation.Certificates (DelegCert (..), MIRCert (..))
import Sophie.Spec.Ledger.EpochBoundary (emptySnapShot)
import Sophie.Spec.Ledger.LedgerState
  ( AccountState (..),
    EpochState (..),
    NewEpochState (..),
    PulsingRewUpdate,
    emptyRewardUpdate,
  )
import Sophie.Spec.Ledger.PParams (PParams' (..))
import Sophie.Spec.Ledger.STS.Bbody (BbodyPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Chain (ChainPredicateFailure (..), ChainState (..))
import Sophie.Spec.Ledger.STS.Deleg (DelegPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Delegs (DelegsPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Delpl (DelplPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Ledger (LedgerPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Ledgers (LedgersPredicateFailure (..))
import Sophie.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (..))
import Sophie.Spec.Ledger.Tx (Tx (..), WitnessSetHKD (..))
import Sophie.Spec.Ledger.TxBody
  ( DCert (..),
    MIRPot (..),
    MIRTarget (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Sophie.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (ExMock, Mock)
import Test.Sophie.Spec.Ledger.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Sophie.Spec.Ledger.Examples.Cast as Cast
import qualified Test.Sophie.Spec.Ledger.Examples.Combinators as C
import Test.Sophie.Spec.Ledger.Examples.Federation
  ( coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
  )
import Test.Sophie.Spec.Ledger.Examples.Init
  ( initSt,
    lastColeHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Sophie.Spec.Ledger.Examples.PoolLifetime (makePulser')
import Test.Sophie.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    NatNonce (..),
    genesisCoins,
    mkBlockFakeVRF,
    mkOCert,
  )
import Test.Sophie.Spec.Ledger.Generator.EraGen (genesisId)
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Test.Sophie.Spec.Ledger.VestedSealUtils (SophieTest, getBlockNonce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

initUTxO :: (SophieTest era) => UTxO era
initUTxO =
  genesisCoins
    genesisId
    [ TxOut Cast.aliceAddr aliceInitCoin,
      TxOut Cast.bobAddr bobInitCoin
    ]
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    bobInitCoin = Val.inject $ Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initStMIR :: forall era. SophieTest era => Coin -> ChainState era
initStMIR treasury = cs {chainNes = (chainNes cs) {nesEs = es'}}
  where
    cs = initSt @era initUTxO
    as = esAccountState . nesEs . chainNes $ cs
    as' =
      as
        { _treasury = (_treasury as) <+> treasury,
          _reserves = (_reserves as) <-> treasury
        }
    es' = (nesEs $ chainNes cs) {esAccountState = as'}

--
-- Block 1, Slot 10, Epoch 0
--

aliceMIRCoin :: Coin
aliceMIRCoin = Coin 100

ir :: CryptoClass.Crypto c => MIRTarget c
ir = StakeAddressesMIR $ Map.fromList [(Cast.aliceSHK, toDeltaCoin aliceMIRCoin)]

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: (SophieTest era) => MIRPot -> TxBody era
txbodyEx1 pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ DCertMir (MIRCert pot ir),
          DCertDeleg (RegKey Cast.aliceSHK)
        ]
    )
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
  where
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000
    aliceCoinEx1 = aliceInitCoin <-> (Val.inject $ feeTx1 <+> _keyDeposit ppEx)

mirWits :: (CryptoClass.Crypto c) => [Int] -> [KeyPair 'Witness c]
mirWits nodes = asWitness <$> map (\x -> cold . coreNodeIssuerKeys $ x) nodes

sufficientMIRWits :: (CryptoClass.Crypto c) => [KeyPair 'Witness c]
sufficientMIRWits = mirWits [0 .. 4]

insufficientMIRWits :: (CryptoClass.Crypto c) => [KeyPair 'Witness c]
insufficientMIRWits = mirWits [0 .. 3]

txEx1 ::
  forall c.
  (Mock c) =>
  [KeyPair 'Witness c] ->
  MIRPot ->
  Tx (SophieEra c)
txEx1 txwits pot =
  Tx
    (txbodyEx1 pot)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx1 @(SophieEra c) pot)
            ([asWitness Cast.alicePay] <> txwits)
      }
    SNothing

blockEx1' ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  [KeyPair 'Witness (Crypto (SophieEra c))] ->
  MIRPot ->
  Block (SophieEra c)
blockEx1' txwits pot =
  mkBlockFakeVRF
    lastColeHeaderHash
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 10)
    [txEx1 txwits pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto (SophieEra c)))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(SophieEra c) ppEx 10) 0 (KESPeriod 0))

blockEx1 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  Block (SophieEra c)
blockEx1 = blockEx1' sufficientMIRWits

expectedStEx1' ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  [KeyPair 'Witness (Crypto (SophieEra c))] ->
  MIRPot ->
  ChainState (SophieEra c)
expectedStEx1' txwits pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1' @c txwits pot))
    . C.newLab (blockEx1' txwits pot)
    . C.feesAndDeposits feeTx1 (_keyDeposit ppEx)
    . C.newUTxO (txbodyEx1 pot)
    . C.newStakeCred Cast.aliceSHK (Ptr (SlotNo 10) 0 1)
    . C.mir Cast.aliceSHK pot aliceMIRCoin
    $ initStMIR (Coin 1000)

expectedStEx1 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  ChainState (SophieEra c)
expectedStEx1 = expectedStEx1' sufficientMIRWits

-- === Block 1, Slot 10, Epoch 0, Successful MIR Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mir1 :: (ExMock (Crypto (SophieEra c))) => MIRPot -> CHAINExample (SophieEra c)
mir1 pot =
  CHAINExample
    (initStMIR (Coin 1000))
    (blockEx1 pot)
    (Right $ expectedStEx1 pot)

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR Wits, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailWits ::
  forall c.
  ( ExMock (Crypto (SophieEra c))
  ) =>
  MIRPot ->
  CHAINExample (SophieEra c)
mirFailWits pot =
  CHAINExample
    (initStMIR (Coin 1000))
    (blockEx1' insufficientMIRWits pot)
    ( Left
        [ BbodyFailure @(SophieEra c)
            ( LedgersFailure
                ( LedgerFailure
                    ( UtxowFailure $
                        MIRInsufficientGenesisSigsUTXOW ws
                    )
                )
            )
        ]
    )
  where
    ws = Set.fromList $ asWitness <$> map (\x -> hk . coreNodeIssuerKeys $ x) [0 .. 3]

-- === Block 1, Slot 10, Epoch 0, Insufficient MIR funds, Reserves Example
--
-- In the first block, submit a MIR cert drawing from the reserves.
mirFailFunds ::
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  Coin ->
  Coin ->
  Coin ->
  CHAINExample (SophieEra c)
mirFailFunds pot treasury llNeeded llReceived =
  CHAINExample
    (initStMIR treasury)
    (blockEx1' sufficientMIRWits pot)
    ( Left
        [ BbodyFailure
            ( LedgersFailure
                ( LedgerFailure
                    ( DelegsFailure
                        ( DelplFailure
                            ( DelegFailure $
                                InsufficientForInstantaneousRewardsDELEG
                                  pot
                                  llNeeded
                                  llReceived
                            )
                        )
                    )
                )
            )
        ]
    )

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  Block (SophieEra c)
blockEx2 pot =
  mkBlockFakeVRF
    (bhHash $ bheader @(SophieEra c) (blockEx1 pot))
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 @(Crypto (SophieEra c)))
    (NatNonce 2)
    minBound
    2
    0
    (mkOCert (coreNodeKeysBySchedule @(SophieEra c) ppEx 50) 0 (KESPeriod 0))

pulserEx2 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  PulsingRewUpdate c
pulserEx2 pot = makePulser' (expectedStEx1 pot)

expectedStEx2 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  ChainState (SophieEra c)
expectedStEx2 pot =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 @c pot))
    . C.newLab (blockEx2 pot)
    . C.pulserUpdate (pulserEx2 pot)
    $ (expectedStEx1 pot)

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to create an empty reward update.
mir2 ::
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  CHAINExample (SophieEra c)
mir2 pot =
  CHAINExample
    (expectedStEx1 pot)
    (blockEx2 pot)
    (Right $ expectedStEx2 pot)

--
-- Block 3, Slot 110, Epoch 1
--

epoch1Nonce ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  Nonce
epoch1Nonce pot = chainCandidateNonce (expectedStEx2 @c pot)

blockEx3 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  Block (SophieEra c)
blockEx3 pot =
  mkBlockFakeVRF
    (bhHash $ bheader @(SophieEra c) (blockEx2 pot))
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (epoch1Nonce @c pot)
    (NatNonce 3)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(SophieEra c) ppEx 110) 0 (KESPeriod 0))

expectedStEx3 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  MIRPot ->
  ChainState (SophieEra c)
expectedStEx3 pot =
  C.newEpoch (blockEx3 pot)
    . C.newSnapshot emptySnapShot feeTx1
    . C.applyRewardUpdate emptyRewardUpdate
    . C.applyMIR pot (Map.singleton Cast.aliceSHK aliceMIRCoin)
    $ (expectedStEx2 pot)

-- === Block 3, Slot 110, Epoch 1
--
-- Submit an empty block in the next epoch to apply the MIR rewards.
mir3 :: (ExMock (Crypto (SophieEra c))) => MIRPot -> CHAINExample (SophieEra c)
mir3 pot = CHAINExample (expectedStEx2 pot) (blockEx3 pot) (Right $ expectedStEx3 pot)

--
-- MIR Test Group
--

mirExample :: TestTree
mirExample =
  testGroup
    "move inst rewards"
    [ testCase "create MIR cert - reserves" $ testCHAINExample (mir1 ReservesMIR),
      testCase "create MIR cert - treasury" $ testCHAINExample (mir1 TreasuryMIR),
      testCase "insufficient MIR witnesses, reserves" $
        testCHAINExample (mirFailWits ReservesMIR),
      testCase "insufficient MIR witnesses, treasury" $
        testCHAINExample (mirFailWits TreasuryMIR),
      testCase "insufficient MIR funds, reserves" $
        testCHAINExample (mirFailFunds ReservesMIR (Coin 34000000000000000) (Coin 100) (Coin 0)),
      testCase "insufficient MIR funds, treasury" $
        testCHAINExample (mirFailFunds TreasuryMIR (Coin 99) (Coin 100) (Coin 99)),
      testCase "end of epoch after MIR - reserves" $
        testCHAINExample (mir2 ReservesMIR),
      testCase "end of epoch after MIR - treasury" $
        testCHAINExample (mir2 TreasuryMIR),
      testCase "apply MIR - reserves" $ testCHAINExample (mir3 ReservesMIR),
      testCase "apply MIR - treasury" $ testCHAINExample (mir3 TreasuryMIR)
    ]
