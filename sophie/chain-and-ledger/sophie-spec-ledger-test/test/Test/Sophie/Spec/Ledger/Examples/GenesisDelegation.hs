{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Sophie.Spec.Ledger.Examples.GenesisDelegation
-- Description : Genesis Delegation Example
--
-- Example demonstrating Genesis Delegation
module Test.Sophie.Spec.Ledger.Examples.GenesisDelegation
  ( genesisDelegExample,
  )
where

import Bcc.Crypto.DSIGN.Class (Signable)
import Bcc.Crypto.Hash (HashAlgorithm)
import qualified Bcc.Crypto.Hash as Hash
import qualified Bcc.Crypto.VRF as VRF
import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Crypto as Cr
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (Crypto (..))
import Bcc.Ledger.Hashes (EraIndependentTxBody)
import Bcc.Ledger.Keys
  ( GenDelegPair (..),
    KeyPair (..),
    KeyRole (..),
    asWitness,
    hashKey,
    hashVerKeyVRF,
  )
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Slot (BlockNo (..), SlotNo (..))
import Bcc.Ledger.Val ((<->))
import qualified Bcc.Ledger.Val as Val
import Bcc.Protocol.TOptimum.BHeader (bhHash)
import Bcc.Protocol.TOptimum.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Sophie.Spec.Ledger.BlockChain (Block, bheader)
import Sophie.Spec.Ledger.LedgerState (FutureGenDeleg (..), PulsingRewUpdate)
import Sophie.Spec.Ledger.PParams (PParams' (..))
import Sophie.Spec.Ledger.STS.Chain (ChainState (..))
import Sophie.Spec.Ledger.Tx (Tx (..), WitnessSet, WitnessSetHKD (..))
import Sophie.Spec.Ledger.TxBody
  ( DCert (..),
    GenesisDelegCert (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Sophie.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (ExMock)
import Test.Sophie.Spec.Ledger.Examples (CHAINExample (..), testCHAINExample)
import qualified Test.Sophie.Spec.Ledger.Examples.Cast as Cast
import qualified Test.Sophie.Spec.Ledger.Examples.Combinators as C
import Test.Sophie.Spec.Ledger.Examples.Federation
  ( coreNodeKeysBySchedule,
    coreNodeSK,
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
  ( NatNonce (..),
    genesisCoins,
    mkBlockFakeVRF,
    mkOCert,
  )
import Test.Sophie.Spec.Ledger.Generator.EraGen (genesisId)
import Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import Test.Sophie.Spec.Ledger.SentryUtils
  ( RawSeed (..),
    SophieTest,
    getBlockNonce,
    mkKeyPair,
    mkVRFKeyPair,
  )
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

initStGenesisDeleg :: forall era. SophieTest era => ChainState era
initStGenesisDeleg = initSt initUTxO

--
-- Block 1, Slot 10, Epoch 0
--

newGenDelegate ::
  CryptoClass.Crypto crypto =>
  KeyPair 'GenesisDelegate crypto
newGenDelegate = KeyPair vkCold skCold
  where
    (skCold, vkCold) = mkKeyPair (RawSeed 108 0 0 0 1)

newGenesisVrfKH ::
  forall h v.
  (HashAlgorithm h, VRF.VRFAlgorithm v) =>
  Hash.Hash h (VRF.VerKeyVRF v)
newGenesisVrfKH = hashVerKeyVRF . snd $ mkVRFKeyPair (RawSeed 9 8 7 6 5)

feeTx1 :: Coin
feeTx1 = Coin 1

txbodyEx1 :: (Cr.Crypto c) => TxBody (SophieEra c)
txbodyEx1 =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx1)
    ( StrictSeq.fromList
        [ DCertGenesis
            ( GenesisDelegCert
                (hashKey (coreNodeVK 0))
                (hashKey (vKey newGenDelegate))
                newGenesisVrfKH
            )
        ]
    )
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing
  where
    aliceCoinEx1 = aliceInitCoin <-> (Val.inject feeTx1)
    aliceInitCoin = Val.inject $ Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

-- fooo :: Bool
-- fooo = addrWits == True

txEx1 ::
  forall c.
  ( CryptoClass.Crypto c,
    -- HashAlgorithm (CryptoClass.HASH c),
    Signable
      (CryptoClass.DSIGN c)
      (Hash.Hash (CryptoClass.HASH c) EraIndependentTxBody)
  ) =>
  Tx (SophieEra c)
txEx1 = Tx txbodyEx1 txwits SNothing
  where
    txwits :: WitnessSet (SophieEra c)
    txwits =
      mempty
        { addrWits =
            makeWitnessesVKey @c
              (hashAnnotated (txbodyEx1 @c))
              ( [asWitness Cast.alicePay]
                  <> [ asWitness $
                         KeyPair @'Genesis @c
                           (coreNodeVK 0)
                           (coreNodeSK @c 0)
                     ]
              )
        }

blockEx1 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  Block (SophieEra c)
blockEx1 =
  mkBlockFakeVRF @(SophieEra c)
    lastColeHeaderHash
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 10)
    [txEx1]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @c)
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert @c (coreNodeKeysBySchedule @(SophieEra c) ppEx 10) 0 (KESPeriod 0))

newGenDeleg ::
  forall crypto.
  CryptoClass.Crypto crypto =>
  (FutureGenDeleg crypto, GenDelegPair crypto)
newGenDeleg =
  ( FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVK 0),
    GenDelegPair (hashKey . vKey $ newGenDelegate) newGenesisVrfKH
  )

expectedStEx1 ::
  forall c.
  (ExMock c) =>
  ChainState (SophieEra c)
expectedStEx1 =
  C.evolveNonceUnfrozen (getBlockNonce @(SophieEra c) (blockEx1))
    . C.newLab blockEx1
    . C.feesAndDeposits feeTx1 (Coin 0)
    . C.newUTxO txbodyEx1
    . C.setFutureGenDeleg newGenDeleg
    $ initStGenesisDeleg

-- === Block 1, Slot 10, Epoch 0
--
-- In the first block, stage a new future genesis delegate
genesisDelegation1 ::
  (ExMock c) =>
  CHAINExample (SophieEra c)
genesisDelegation1 = CHAINExample initStGenesisDeleg blockEx1 (Right expectedStEx1)

--
-- Block 2, Slot 50, Epoch 0
--

blockEx2 ::
  forall c.
  (ExMock (Crypto (SophieEra c))) =>
  Block (SophieEra c)
blockEx2 =
  mkBlockFakeVRF @(SophieEra c)
    (bhHash $ bheader blockEx1)
    (coreNodeKeysBySchedule @(SophieEra c) ppEx 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 @c)
    (NatNonce 2)
    minBound
    2
    0
    (mkOCert @c (coreNodeKeysBySchedule @(SophieEra c) ppEx 50) 0 (KESPeriod 0))

pulserEx2 :: forall c. (ExMock c, C.UsesPP (SophieEra c)) => PulsingRewUpdate c
pulserEx2 = makePulser' expectedStEx1

expectedStEx2 ::
  forall c.
  (ExMock c, C.UsesPP (SophieEra c)) =>
  ChainState (SophieEra c)
expectedStEx2 =
  C.evolveNonceUnfrozen (getBlockNonce @(SophieEra c) blockEx2)
    . C.newLab blockEx2
    . C.adoptFutureGenDeleg newGenDeleg
    . C.pulserUpdate pulserEx2
    $ expectedStEx1

-- === Block 2, Slot 50, Epoch 0
--
-- Submit an empty block to trigger adopting the genesis delegation.
genesisDelegation2 ::
  (ExMock c, C.UsesPP (SophieEra c)) =>
  CHAINExample (SophieEra c)
genesisDelegation2 = CHAINExample expectedStEx1 blockEx2 (Right expectedStEx2)

--
-- Genesis Delegation Test Group
--

genesisDelegExample :: TestTree
genesisDelegExample =
  testGroup
    "genesis delegation"
    [ testCase "stage genesis key delegation" $ testCHAINExample genesisDelegation1,
      testCase "adopt genesis key delegation" $ testCHAINExample genesisDelegation2
    ]
