{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Sophie.Spec.Ledger.Examples.Init
-- Description : Initial State for Sophie ledger examples
--
-- The initial state for Sophie Ledger Examples.
module Test.Sophie.Spec.Ledger.Examples.Init
  ( ppEx,
    initSt,
    nonce0,
    lastColeHeaderHash,
  )
where

import Bcc.Ledger.BaseTypes
  ( Nonce (..),
  )
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (Crypto)
import Bcc.Ledger.Slot
  ( BlockNo (..),
    EpochNo (..),
    SlotNo (..),
  )
import Bcc.Ledger.Val ((<->))
import qualified Bcc.Ledger.Val as Val
import Bcc.Protocol.TOptimum.BHeader
  ( HashHeader (..),
    LastAppliedBlock (..),
    hashHeaderToNonce,
  )
import Bcc.Slotting.Slot (WithOrigin (..))
import Sophie.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    emptyPParams,
  )
import Sophie.Spec.Ledger.STS.Chain
  ( ChainState (..),
    initialSophieState,
  )
import Sophie.Spec.Ledger.UTxO (UTxO (..), balance)
import Test.Sophie.Spec.Ledger.Examples.Federation (genDelegs)
import Test.Sophie.Spec.Ledger.VestedSealUtils (SophieTest, maxLLSupply, mkHash, unsafeBoundRational)

-- === Initial Protocol Parameters
--
-- @
--   emptyPParams
--     { _maxBBSize = 50000,
--       _maxBHSize = 10000,
--       _maxTxSize = 10000,
--       _eMax = EpochNo 10000,
--       _keyDeposit = Coin 7,
--       _poolDeposit = Coin 250,
--       _d = unsafeBoundRational 0.5,
--       _tau = unsafeBoundRational 0.2,
--       _rho = unsafeBoundRational 0.0021,
--       _minUTxOValue = 100
--     }
-- @
ppEx :: PParams era
ppEx =
  emptyPParams
    { _maxBBSize = 50000,
      _maxBHSize = 10000,
      _maxTxSize = 10000,
      _eMax = EpochNo 10000,
      _keyDeposit = Coin 7,
      _poolDeposit = Coin 250,
      _d = unsafeBoundRational 0.5,
      _tau = unsafeBoundRational 0.2,
      _rho = unsafeBoundRational 0.0021,
      _minUTxOValue = Coin 100
    }

-- | === The hash of the last Bryon Header
--
-- The first block of the Sophie era will point back to the
-- last block of the Cole era.
-- For our purposes in the examples we can bootstrap the chain
-- by just coercing the value.
-- When this transition actually occurs,
-- the consensus layer will do the work of making
-- sure that the hash gets translated across the fork.
lastColeHeaderHash ::
  forall c.
  CryptoClass.Crypto c =>
  HashHeader c
lastColeHeaderHash = HashHeader $ mkHash 0

-- | === Initial Nonce
nonce0 ::
  forall c.
  CryptoClass.Crypto c =>
  Nonce
nonce0 = hashHeaderToNonce (lastColeHeaderHash @c)

-- | === Initial Chain State
--
-- The initial state for the examples uses the function
-- 'initialSophieState' with the genesis delegation
-- 'genDelegs' and any given starting 'UTxO' set.
initSt :: forall era. SophieTest era => UTxO era -> ChainState era
initSt utxo =
  initialSophieState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastColeHeaderHash)
    (EpochNo 0)
    utxo
    (maxLLSupply <-> (Val.coin $ balance utxo))
    genDelegs
    ppEx
    (nonce0 @(Crypto era))
