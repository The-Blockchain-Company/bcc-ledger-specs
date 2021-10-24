{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Arrows #-}

module Sophie.Spec.Ledger.SentryOps( 
  SentryOpen (..),
  Sentry (..),
  DiffTime (..),
  poolCalc,
)where

#if defined(__GLASGOW_HASKELL__) && !defined(mingw32_HOST_OS) && !defined(__GHCJS__)
import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)
#endif

import qualified System.Timeout as IO
import qualified Sophie.Spec.Ledger.EpochBoundary(aggregateUtxoCoinByCredential)
import qualified Sophie.Spec.Ledger.API.Wallet(getPools)
import Bcc.Binary (ToCBOR (..), decodeFull, decodeFullDecoder, serialize)
import Bcc.Crypto.DSIGN.Class (decodeSignedDSIGN, sizeSigDSIGN, sizeVerKeyDSIGN)
import qualified Bcc.Crypto.VRF as VRF
import Bcc.Ledger.Address (Addr (..))
import Bcc.Ledger.BaseTypes (Globals (..), NonNegativeInterval, Seed, UnitInterval, epochInfo)
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Credential (Credential, Ptr, StakeReference (..))
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Serialization (decodeRecordNamed)
import Bcc.Ledger.Val ((<+>), (<×>), (<->))
import qualified Bcc.Ledger.Val as Val
import Bcc.Ledger.Era (Crypto, Era)
import Bcc.Ledger.Keys (KeyHash, KeyRole (..), SignKeyVRF)
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Sophie.Constraints (UsesValue)
import Bcc.Ledger.Slot (epochInfoSize)
import Control.DeepSeq (NFData)
import Control.SetAlgebra (dom, eval, setSingleton, (▷), (◁))
import Data.Aeson
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ratio ((%))
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet
import Sophie.Spec.Ledger.TxBody (PoolParams)
import Sophie.Spec.Ledger.UTxO (UTxO (..))
import Data.Default.Class (Default (..))
import Data.Either (fromRight)
import Data.Foldable (fold)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set
import GHC.Records (HasField (..), getField)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Data.List hiding ((!!))
import Sophie.Spec.Ledger.API.Protocol (ChainDepState (..))
import Sophie.Spec.Ledger.BlockChain (checkLeaderValue, mkSeed, seedL)
import Sophie.Spec.Ledger.CompactAddr (CompactAddr, compactAddr)
import Control.Monad.Trans.Reader (runReader)
import Control.Provenance (runWithProvM)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)

import           Control.Applicative (Alternative((<|>)))
import qualified Control.Concurrent as IO
import           Control.Exception (assert)
import           Control.Monad.Reader
import           Data.Kind (Type)
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Set (Set, lookupMin, lookupMax, elemAt, member)
import Data.Maybe (listToMaybe)
import Data.Fixed
import Sophie.Spec.Ledger.LedgerState
  ( DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    RewardUpdate,
    UTxOState (..),
    circulation,
    consumed,
    createRUpd,
    minfee,
    produced,
    stakeDistr,
  )

-----------------------------------------------------------------------------------------------
  ---code--Proxies, sentrys, time counting monads, file locks. 
-------- --------------------------------------------------------------------------------------
data Sentry = TotalPools | SentryVal | SentryIndx | SentryList| SentryVar deriving (Eq, Ord, Show)

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

data SentryOpen = SentryVerify Int Int | SentryElem Int deriving (Eq, Ord, Show)

data Set a = Set [a] deriving (Eq, Ord, Show)

xs :: [Int]
xs = [7777, 54369, 381073, 2667511, 18672577, 914956273]

map1 = (\(x)-> M.size x)
indx = (\(x) -> xs (!! 0 ))

poolCalc :: ( a -> b) -> a -> b
poolCalc f x = f x 

find :: (a -> Bool) -> [a] -> Maybe a
find p =  listToMaybe . filter p

map12 = (\(x) y z -> (!!)[y]z)
 
watch :: Bool -> String
watch b = show b ++ " Bcc pools are in existence " ++ 
  case b of 
    True -> "...The next sentry has now opened!"
    False -> "...The next sentry remains closed" ++ truth
      where truth = 
              "Gods will is done"
lie = 
  "in time he shall"
