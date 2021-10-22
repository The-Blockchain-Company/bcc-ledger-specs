{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Bcc.Chain.Genesis.Initializer
  ( GenesisInitializer (..),
    TestnetBalanceOptions (..),
    FakeAvvmOptions (..),
  )
where

import Bcc.Chain.Common (Entropic)
import Bcc.Prelude

-- | Options determining generated genesis stakes, balances, and delegation
data GenesisInitializer = GenesisInitializer
  { giTestBalance :: !TestnetBalanceOptions,
    giFakeAvvmBalance :: !FakeAvvmOptions,
    -- | Avvm balances will be multiplied by this factor
    giAvvmBalanceFactor :: !Rational,
    -- | Whether to use heavyweight delegation for genesis keys
    giUseHeavyDlg :: !Bool
  }
  deriving (Eq, Show)

-- | These options determine balances of nodes specific for testnet
data TestnetBalanceOptions = TestnetBalanceOptions
  { -- | Number of poor nodes (with small balance).
    tboPoors :: !Word,
    -- | Number of rich nodes (with huge balance).
    tboRichmen :: !Word,
    -- | Total balance owned by these nodes.
    tboTotalBalance :: !Entropic,
    -- | Portion of stake owned by all richmen together.
    tboRichmenShare :: !Rational
  }
  deriving (Eq, Show)

-- | These options determines balances of fake AVVM nodes which didn't really go
--   through vending, but pretend they did
data FakeAvvmOptions = FakeAvvmOptions
  { faoCount :: !Word,
    faoOneBalance :: !Entropic
  }
  deriving (Eq, Show, Generic)
