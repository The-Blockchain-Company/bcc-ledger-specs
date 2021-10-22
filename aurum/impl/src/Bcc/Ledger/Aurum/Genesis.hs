{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bcc.Ledger.Aurum.Genesis
  ( AurumGenesis (..),
    extendPPWithGenesis,

    -- * Deprecated
    bccPerUTxOWord,
  )
where

import Bcc.Binary
import Bcc.Ledger.Aurum.Language (Language)
import Bcc.Ledger.Aurum.PParams
import Bcc.Ledger.Aurum.Scripts
import Bcc.Ledger.Coin
import Data.Coders
import Data.Functor.Identity
import Data.Map.Strict
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural
import qualified Sophie.Spec.Ledger.PParams as Sophie

data AurumGenesis = AurumGenesis
  { coinsPerUTxOWord :: !Coin,
    costmdls :: !(Map Language CostModel),
    prices :: !Prices,
    maxTxExUnits :: !ExUnits,
    maxBlockExUnits :: !ExUnits,
    maxValSize :: !Natural,
    collateralPercentage :: !Natural,
    maxCollateralInputs :: !Natural
  }
  deriving (Eq, Generic, NoThunks)

{-# DEPRECATED bccPerUTxOWord "Use coinsPerUTxOWord instead" #-}
bccPerUTxOWord :: AurumGenesis -> Coin
bccPerUTxOWord = coinsPerUTxOWord

-- | Given the missing pieces turn a Sophie.PParams' into an Params'
extendPPWithGenesis ::
  Sophie.PParams' Identity era1 ->
  AurumGenesis ->
  PParams' Identity era2
extendPPWithGenesis
  pp
  AurumGenesis
    { coinsPerUTxOWord,
      costmdls,
      prices,
      maxTxExUnits,
      maxBlockExUnits,
      maxValSize,
      collateralPercentage,
      maxCollateralInputs
    } =
    extendPP
      pp
      coinsPerUTxOWord
      costmdls
      prices
      maxTxExUnits
      maxBlockExUnits
      maxValSize
      collateralPercentage
      maxCollateralInputs

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance FromCBOR AurumGenesis where
  fromCBOR =
    decode $
      RecD AurumGenesis
        <! From
        <! D decodeCostModelMap
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance ToCBOR AurumGenesis where
  toCBOR
    AurumGenesis
      { coinsPerUTxOWord,
        costmdls,
        prices,
        maxTxExUnits,
        maxBlockExUnits,
        maxValSize,
        collateralPercentage,
        maxCollateralInputs
      } =
      encode $
        Rec AurumGenesis
          !> To coinsPerUTxOWord
          !> To costmdls
          !> To prices
          !> To maxTxExUnits
          !> To maxBlockExUnits
          !> To maxValSize
          !> To collateralPercentage
          !> To maxCollateralInputs
