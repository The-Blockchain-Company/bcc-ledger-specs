{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Sophie.Spec.Ledger.Examples.Updates
-- Description : Protocol Parameter Update Example
--
-- Example demonstrating using the protocol parameter update system.
module Test.Sophie.Spec.Ledger.Examples.Updates
  ( updatesExample,
  )
where

import Bcc.Ledger.BaseTypes
  ( Nonce,
    StrictMaybe (..),
    mkNonceFromNumber,
    (⭒),
  )
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Crypto as Cr
import Bcc.Ledger.Era (Crypto (..))
import Bcc.Ledger.Keys (asWitness, hashKey)
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot
  ( BlockNo (..),
    EpochNo (..),
    SlotNo (..),
  )
import Bcc.Ledger.Val ((<+>), (<->))
import qualified Bcc.Ledger.Val as Val
import Bcc.Protocol.TOptimum.BHeader (bhHash)
import Bcc.Protocol.TOptimum.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Sophie.Spec.Ledger.BlockChain (Block, bheader)
import qualified Sophie.Spec.Ledger.EpochBoundary as EB
import Sophie.Spec.Ledger.LedgerState (PulsingRewUpdate, emptyRewardUpdate)
import Sophie.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    ProposedPPUpdates (..),
    Update (..),
  )
import Sophie.Spec.Ledger.STS.Chain (ChainState (..))
import Sophie.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Sophie.Spec.Ledger.TxBody
  ( TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Sophie.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (ExMock)
import Test.Sophie.Spec.Ledger.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Sophie.Spec.Ledger.Examples.Cast as Cast
import qualified Test.Sophie.Spec.Ledger.Examples.Combinators as C
import Test.Sophie.Spec.Ledger.Examples.Federation
  ( coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
    coreNodeVK,
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
import Test.Sophie.Spec.Ledger.VestedSealUtils (getBlockNonce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

initUTxO :: Cr.Crypto c => UTxO (SophieEra c)
initUTxO =
  genesisCoins
    genesisId
    [ TxOut Cast.aliceAddr (Val.inject aliceInitCoin),
      TxOut Cast.bobAddr (Val.inject bobInitCoin)
    ]

initStUpdates :: Cr.Crypto c => ChainState (SophieEra c)
initStUpdates = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

ppVoteA :: PParamsUpdate (SophieEra c)
ppVoteA =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _poolDeposit = SJust $ Coin 200,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SJust (mkNonceFromNumber 123),
      _protocolVersion = SNothing,
      _minUTxOValue = SNothing,
      _minPoolCost = SNothing
    }

collectVotes :: Era (SophieEra c) => PParamsUpdate (SophieEra c) -> [Int] -> ProposedPPUpdates (SophieEra c)
collectVotes vote =
  ProposedPPUpdates . Map.fromList . (fmap (\n -> (hashKey $ coreNodeVK n, vote)))

ppVotes1 :: Era (SophieEra c) => ProposedPPUpdates (SophieEra c)
ppVotes1 = collectVotes ppVoteA [0, 3, 4]

feeTx1 :: Coin
feeTx1 = Coin 1

aliceCoinEx1 :: Coin
aliceCoinEx1 = aliceInitCoin <-> feeTx1

txbodyEx1 :: Cr.Crypto c => TxBody (SophieEra c)
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (Val.inject aliceCoinEx1))
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx1
    (SlotNo 10)
    (SJust (Update ppVotes1 (EpochNo 0)))
    SNothing

txEx1 :: forall c. (ExMock (Crypto (SophieEra c))) => Tx (SophieEra c)
txEx1 =
  Tx
    txbodyEx1
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx1 @c)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeIssuerKeys 0,
                     asWitness . cold $ coreNodeIssuerKeys 3,
                     asWitness . cold $ coreNodeIssuerKeys 4
                   ]
            )
      }
    SNothing

blockEx1 :: forall c. (ExMock (Crypto (SophieEra c))) => Block (SophieEra c)
blockEx1 =
  mkBlockFakeVRF
    lastColeHeaderHash
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto (SophieEra c)))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @(SophieEra c) ppEx 10) 0 (KESPeriod 0))

expectedStEx1 :: forall c. (ExMock (Crypto (SophieEra c))) => ChainState (SophieEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx1 @c))
    . C.newLab blockEx1
    . C.feesAndDeposits feeTx1 (Coin 0)
    . C.newUTxO txbodyEx1
    . C.setCurrentProposals ppVotes1
    $ initStUpdates

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, three genesis keys vote on the same new parameters.
updates1 :: (ExMock (Crypto (SophieEra c))) => CHAINExample (SophieEra c)
updates1 = CHAINExample initStUpdates blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 20, Epoch 0
--

ppVotes2 :: Era (SophieEra c) => ProposedPPUpdates (SophieEra c)
ppVotes2 = collectVotes ppVoteA [1, 5]

updateEx3B :: Era (SophieEra c) => Update (SophieEra c)
updateEx3B = Update ppVotes2 (EpochNo 0)

feeTx2 :: Coin
feeTx2 = Coin 1

aliceCoinEx2 :: Coin
aliceCoinEx2 = aliceCoinEx1 <-> feeTx2

txbodyEx2 :: forall c. Cr.Crypto c => TxBody (SophieEra c)
txbodyEx2 =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx1) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (Val.inject aliceCoinEx2))
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 31)
    (SJust updateEx3B)
    SNothing

txEx2 :: forall c. (ExMock (Crypto (SophieEra c))) => Tx (SophieEra c)
txEx2 =
  Tx
    txbodyEx2
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx2 @c)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeIssuerKeys 1,
                     asWitness . cold $ coreNodeIssuerKeys 5
                   ]
            )
      }
    SNothing

blockEx2 :: forall c. (Cr.Crypto c, ExMock (Crypto (SophieEra c))) => Block (SophieEra c)
blockEx2 =
  mkBlockFakeVRF
    (bhHash $ bheader @(SophieEra c) blockEx1)
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 20)
    [txEx2]
    (SlotNo 20)
    (BlockNo 2)
    (nonce0 @(Crypto (SophieEra c)))
    (NatNonce 2)
    minBound
    1
    0
    (mkOCert (coreNodeKeysBySchedule @(SophieEra c) ppEx 20) 0 (KESPeriod 0))

expectedStEx2 :: forall c. (ExMock (Crypto (SophieEra c))) => ChainState (SophieEra c)
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce (blockEx2 @c))
    . C.newLab blockEx2
    . C.feesAndDeposits feeTx2 (Coin 0)
    . C.newUTxO txbodyEx2
    . C.setCurrentProposals (collectVotes ppVoteA [0, 1, 3, 4, 5])
    $ expectedStEx1

-- === Block 2, Slot 20, Epoch 0
--
-- In the second block, two more genesis keys vote for the same new parameters.
updates2 :: (ExMock (Crypto (SophieEra c))) => CHAINExample (SophieEra c)
updates2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Block 3, Slot 80, Epoch 0
--

ppVoteB :: PParamsUpdate (SophieEra c)
ppVoteB =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _poolDeposit = SNothing,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SNothing,
      _protocolVersion = SNothing,
      _minUTxOValue = SJust $ Coin 99,
      _minPoolCost = SNothing
    }

ppVotes3 :: Era (SophieEra c) => ProposedPPUpdates (SophieEra c)
ppVotes3 = collectVotes ppVoteB [1]

feeTx3 :: Coin
feeTx3 = Coin 1

aliceCoinEx3 :: Coin
aliceCoinEx3 = aliceCoinEx2 <-> feeTx3

txbodyEx3 :: forall c. Cr.Crypto c => TxBody (SophieEra c)
txbodyEx3 =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx2) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (Val.inject aliceCoinEx3))
    StrictSeq.empty
    (Wdrl Map.empty)
    feeTx3
    (SlotNo 81)
    (SJust (Update ppVotes3 (EpochNo 1)))
    SNothing

txEx3 :: forall c. (Cr.Crypto c, ExMock (Crypto (SophieEra c))) => Tx (SophieEra c)
txEx3 =
  Tx
    txbodyEx3
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx3 @c)
            [asWitness Cast.alicePay, asWitness . cold $ coreNodeIssuerKeys 1]
      }
    SNothing

blockEx3 :: forall c. (Cr.Crypto c, ExMock (Crypto (SophieEra c))) => Block (SophieEra c)
blockEx3 =
  mkBlockFakeVRF
    (bhHash $ bheader @(SophieEra c) blockEx2)
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 80)
    [txEx3]
    (SlotNo 80)
    (BlockNo 3)
    (nonce0 @(Crypto (SophieEra c)))
    (NatNonce 3)
    minBound
    4
    0
    (mkOCert (coreNodeKeysBySchedule @(SophieEra c) ppEx 80) 0 (KESPeriod 0))

pulserEx3 :: forall c. (ExMock c) => PulsingRewUpdate c
pulserEx3 = makePulser' expectedStEx2

expectedStEx3 :: forall c. (ExMock (Crypto (SophieEra c))) => ChainState (SophieEra c)
expectedStEx3 =
  C.evolveNonceFrozen (getBlockNonce (blockEx3 @c))
    . C.newLab blockEx3
    . C.feesAndDeposits feeTx3 (Coin 0)
    . C.newUTxO txbodyEx3
    . C.pulserUpdate pulserEx3
    . C.setFutureProposals (collectVotes ppVoteB [1])
    $ expectedStEx2

-- === Block 3, Slot 80, Epoch 0
--
-- In the third block, one genesis keys votes for the next epoch
updates3 :: (ExMock (Crypto (SophieEra c))) => CHAINExample (SophieEra c)
updates3 = CHAINExample expectedStEx2 blockEx3 (Right expectedStEx3)

--
-- Block 4, Slot 110, Epoch 1
--

epoch1Nonce :: forall c. (ExMock (Crypto (SophieEra c))) => Nonce
epoch1Nonce = (chainCandidateNonce (expectedStEx3 @c)) ⭒ mkNonceFromNumber 123

blockEx4 :: forall c. (Cr.Crypto c, ExMock (Crypto (SophieEra c))) => Block (SophieEra c)
blockEx4 =
  mkBlockFakeVRF
    (bhHash $ bheader @(SophieEra c) blockEx3)
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 110)
    []
    (SlotNo 110)
    (BlockNo 4)
    (epoch1Nonce @c)
    (NatNonce 4)
    minBound
    5
    0
    (mkOCert (coreNodeKeysBySchedule @(SophieEra c) ppEx 110) 0 (KESPeriod 0))

ppExUpdated :: PParams (SophieEra c)
ppExUpdated = ppEx {_poolDeposit = Coin 200, _extraEntropy = mkNonceFromNumber 123}

expectedStEx4 :: forall c. (ExMock (Crypto (SophieEra c))) => ChainState (SophieEra c)
expectedStEx4 =
  C.newEpoch blockEx4
    . C.newSnapshot EB.emptySnapShot (feeTx1 <+> feeTx2 <+> feeTx3)
    . C.applyRewardUpdate emptyRewardUpdate
    . C.setCurrentProposals (collectVotes ppVoteB [1])
    . C.setFutureProposals (ProposedPPUpdates Map.empty)
    . C.setPParams ppExUpdated
    $ expectedStEx3

-- === Block 4, Slot 110, Epoch 1
--
-- In the fourth block, the new protocol parameters are adopted,
-- and the future vote becomes a current vote.
-- Since the extra entropy was voted on, notice that it is a part
-- of the new epoch nonce.
updates4 :: (ExMock (Crypto (SophieEra c))) => CHAINExample (SophieEra c)
updates4 = CHAINExample expectedStEx3 blockEx4 (Right expectedStEx4)

--
-- Updates Test Group
--

updatesExample :: TestTree
updatesExample =
  testGroup
    "protocol parameter updates"
    [ testCase "get 3/7 votes for a pparam update" $ testCHAINExample updates1,
      testCase "get 5/7 votes for a pparam update" $ testCHAINExample updates2,
      testCase "votes for the next epoch" $ testCHAINExample updates3,
      testCase "processes a pparam update" $ testCHAINExample updates4
    ]
