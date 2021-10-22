{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sophie.Spec.Ledger.Examples.EmptyBlock
  ( exEmptyBlock,
  )
where

import Bcc.Ledger.BaseTypes (Nonce)
import Bcc.Ledger.Era (Crypto (..))
import Bcc.Ledger.Slot
  ( BlockNo (..),
    SlotNo (..),
  )
import Bcc.Protocol.TOptimum.OCert (KESPeriod (..))
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import Sophie.Spec.Ledger.BlockChain (Block)
import Sophie.Spec.Ledger.STS.Chain (ChainState (..))
import Sophie.Spec.Ledger.UTxO (UTxO (..))
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (ExMock)
import Test.Sophie.Spec.Ledger.Examples (CHAINExample (..))
import Test.Sophie.Spec.Ledger.Examples.Combinators
  ( evolveNonceUnfrozen,
    newLab,
  )
import Test.Sophie.Spec.Ledger.Examples.Federation (coreNodeKeysBySchedule)
import Test.Sophie.Spec.Ledger.Examples.Init
  ( initSt,
    lastColeHeaderHash,
    nonce0,
    ppEx,
  )
import Test.Sophie.Spec.Ledger.Generator.Core
  ( NatNonce (..),
    PreAurum,
    mkBlockFakeVRF,
    mkOCert,
  )
import Test.Sophie.Spec.Ledger.VestedSealUtils (SophieTest, getBlockNonce)

-- =============================================================

initStEx1 :: forall era. SophieTest era => ChainState era
initStEx1 = initSt (UTxO Map.empty)

blockEx1 ::
  forall era.
  ( HasCallStack,
    SophieTest era,
    ExMock (Crypto era)
  ) =>
  Block era
blockEx1 =
  mkBlockFakeVRF
    lastColeHeaderHash
    (coreNodeKeysBySchedule @era ppEx 10)
    []
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 @(Crypto era))
    (NatNonce 1)
    minBound
    0
    0
    (mkOCert (coreNodeKeysBySchedule @era ppEx 10) 0 (KESPeriod 0))

blockNonce ::
  forall era.
  ( HasCallStack,
    PreAurum era,
    SophieTest era,
    ExMock (Crypto era)
  ) =>
  Nonce
blockNonce = getBlockNonce (blockEx1 @era)

expectedStEx1 ::
  forall era.
  (SophieTest era, ExMock (Crypto era), PreAurum era) =>
  ChainState era
expectedStEx1 =
  (evolveNonceUnfrozen (blockNonce @era))
    . (newLab blockEx1)
    $ initStEx1

-- | = Empty Block Example
--
-- This is the most minimal example of using the CHAIN STS transition.
-- It applies an empty block to an initial sophie chain state.
--
-- The only things that change in the chain state are the
-- evolving and candidate nonces, and the last applied block.
exEmptyBlock :: (SophieTest era, ExMock (Crypto era), PreAurum era) => CHAINExample era
exEmptyBlock = CHAINExample initStEx1 blockEx1 (Right expectedStEx1)
