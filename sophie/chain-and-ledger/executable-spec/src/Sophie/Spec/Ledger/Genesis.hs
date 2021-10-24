{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sophie.Spec.Ledger.Genesis
  ( SophieGenesisStaking (..),
    SophieGenesis (..),
    ValidationErr (..),
    emptyGenesisStaking,
    sgActiveSlotCoeff,
    genesisUTxO,
    initialFundsPseudoTxIn,
    validateGenesis,
    describeValidationErr,
    mkSophieGlobals,
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import qualified Bcc.Crypto.Hash.Class as Crypto
import Bcc.Crypto.KES.Class (totalPeriodsKES)
import Bcc.Ledger.Address
import Bcc.Ledger.BaseTypes
import Bcc.Ledger.Coin
import Bcc.Ledger.Crypto (HASH, KES)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era
import Bcc.Ledger.Hashes (EraIndependentTxBody)
import Bcc.Ledger.Keys
import Bcc.Ledger.SafeHash (unsafeMakeSafeHash)
import Bcc.Ledger.Serialization
  ( decodeRecordNamed,
    mapFromCBOR,
    mapToCBOR,
    utcTimeFromCBOR,
    utcTimeToCBOR,
  )
import Bcc.Ledger.Sophie.Constraints (UsesTxOut (..))
import qualified Bcc.Ledger.Val as Val
import Bcc.Prelude (forceElemsToWHNF)
import Bcc.Slotting.EpochInfo
import Bcc.Slotting.Slot (EpochSize (..))
import Bcc.Slotting.Time (SystemStart (SystemStart))
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime (..))
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import NoThunks.Class (NoThunks (..))
import Sophie.Spec.Ledger.PParams
import Sophie.Spec.Ledger.StabilityWindow
import Sophie.Spec.Ledger.TxBody (PoolParams (..), TxId (..), TxIn (..))
import Sophie.Spec.Ledger.UTxO

-- | Genesis Sophie staking configuration.
--
-- This allows us to configure some initial stake pools and delegation to them,
-- in order to test Optimum in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data SophieGenesisStaking crypto = SophieGenesisStaking
  { -- | Pools to register
    --
    --   The key in this map is the hash of the public key of the _pool_. This
    --   need not correspond to any payment or staking key, but must correspond
    --   to the cold key held by 'TOptimumIsCoreNode'.
    sgsPools :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | Stake-holding key hash credentials and the pools to delegate that stake
    -- to. We require the raw staking key hash in order to:
    --
    -- - Avoid pointer addresses, which would be tricky when there's no slot or
    --   transaction to point to.
    -- - Avoid script credentials.
    sgsStake :: !(Map (KeyHash 'Staking crypto) (KeyHash 'StakePool crypto))
  }
  deriving stock (Eq, Show, Generic)

instance NoThunks (SophieGenesisStaking crypto)

instance CC.Crypto crypto => ToCBOR (SophieGenesisStaking crypto) where
  toCBOR (SophieGenesisStaking pools stake) =
    encodeListLen 2 <> mapToCBOR pools <> mapToCBOR stake

instance CC.Crypto crypto => FromCBOR (SophieGenesisStaking crypto) where
  fromCBOR = do
    decodeRecordNamed "SophieGenesisStaking" (const 2) $ do
      pools <- mapFromCBOR
      stake <- mapFromCBOR
      pure $ SophieGenesisStaking pools stake

-- | Empty genesis staking
emptyGenesisStaking :: SophieGenesisStaking crypto
emptyGenesisStaking =
  SophieGenesisStaking
    { sgsPools = Map.empty,
      sgsStake = Map.empty
    }

-- | Sophie genesis information
--
-- Note that this is needed only for a pure Sophie network, hence it being
-- defined here rather than in its own module. In mainnet, Sophie will
-- transition naturally from Cole, and thus will never have its own genesis
-- information.
data SophieGenesis era = SophieGenesis
  { sgSystemStart :: !UTCTime,
    sgNetworkMagic :: !Word32,
    sgNetworkId :: !Network,
    sgActiveSlotsCoeff :: !PositiveUnitInterval,
    sgSecurityParam :: !Word64,
    sgVestMultiple :: !Word64, 
    sgEpochLength :: !EpochSize,
    sgSlotsPerKESPeriod :: !Word64,
    sgMaxKESEvolutions :: !Word64,
    sgSlotLength :: !NominalDiffTime,
    sgUpdateQuorum :: !Word64,
    sgMaxEntropicSupply :: !Word64,
    sgProtocolParams :: !(PParams era),
    sgGenDelegs :: !(Map (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era))),
    sgVestedDelegs :: !(Map (KeyHash 'Vested (Crypto era)) (VestedDelegPair (Crypto era))),
    sgInitialFunds :: !(Map (Addr (Crypto era)) Coin),
    sgStaking :: !(SophieGenesisStaking (Crypto era))
  }
  deriving stock (Eq, Show, Generic)

deriving instance Era era => NoThunks (SophieGenesis era)

sgActiveSlotCoeff :: SophieGenesis era -> ActiveSlotCoeff
sgActiveSlotCoeff = mkActiveSlotCoeff . sgActiveSlotsCoeff

instance Era era => ToJSON (SophieGenesis era) where
  toJSON sg =
    Aeson.object
      [ "systemStart" .= sgSystemStart sg,
        "networkMagic" .= sgNetworkMagic sg,
        "networkId" .= sgNetworkId sg,
        "activeSlotsCoeff" .= sgActiveSlotsCoeff sg,
        "securityParam" .= sgSecurityParam sg,
        "vestMultiple" .= sgVestMultiple sg,
        "epochLength" .= sgEpochLength sg,
        "slotsPerKESPeriod" .= sgSlotsPerKESPeriod sg,
        "maxKESEvolutions" .= sgMaxKESEvolutions sg,
        "slotLength" .= sgSlotLength sg,
        "updateQuorum" .= sgUpdateQuorum sg,
        "maxEntropicSupply" .= sgMaxEntropicSupply sg,
        "protocolParams" .= sgProtocolParams sg,
        "genDelegs" .= sgGenDelegs sg,
        "vestedDelegs" .= sgVestedDelegs sg,
        "initialFunds" .= sgInitialFunds sg,
        "staking" .= sgStaking sg
      ]

instance Era era => FromJSON (SophieGenesis era) where
  parseJSON =
    Aeson.withObject "SophieGenesis" $ \obj ->
      SophieGenesis
        <$> (forceUTCTime <$> obj .: "systemStart")
        <*> obj .: "networkMagic"
        <*> obj .: "networkId"
        <*> obj .: "activeSlotsCoeff"
        <*> obj .: "securityParam"
        <*> obj .: "vestMultiple"
        <*> obj .: "epochLength"
        <*> obj .: "slotsPerKESPeriod"
        <*> obj .: "maxKESEvolutions"
        <*> obj .: "slotLength"
        <*> obj .: "updateQuorum"
        <*> obj .: "maxEntropicSupply"
        <*> obj .: "protocolParams"
        <*> (forceElemsToWHNF <$> obj .: "genDelegs")
        <*> (forceElemsToWHNF <$> obj .: "vestedDelegs")
        <*> (forceElemsToWHNF <$> obj .: "initialFunds")
        <*> obj .:? "staking" .!= emptyGenesisStaking
    where
      forceUTCTime date =
        let !day = utctDay date
            !time = utctDayTime date
         in UTCTime day time

instance CC.Crypto crypto => ToJSON (SophieGenesisStaking crypto) where
  toJSON sgs =
    Aeson.object
      [ "pools" .= sgsPools sgs,
        "stake" .= sgsStake sgs
      ]

instance CC.Crypto crypto => FromJSON (SophieGenesisStaking crypto) where
  parseJSON =
    Aeson.withObject "SophieGenesisStaking" $ \obj ->
      SophieGenesisStaking
        <$> (forceElemsToWHNF <$> obj .: "pools")
        <*> (forceElemsToWHNF <$> obj .: "stake")

instance Era era => ToCBOR (SophieGenesis era) where
  toCBOR
    SophieGenesis
      { sgSystemStart,
        sgNetworkMagic,
        sgNetworkId,
        sgActiveSlotsCoeff,
        sgSecurityParam,
        sgVestMultiple,
        sgEpochLength,
        sgSlotsPerKESPeriod,
        sgMaxKESEvolutions,
        sgSlotLength,
        sgUpdateQuorum,
        sgMaxEntropicSupply,
        sgProtocolParams,
        sgGenDelegs,
        sgVestedDelegs,
        sgInitialFunds,
        sgStaking
      } =
      encodeListLen 17
        <> utcTimeToCBOR sgSystemStart
        <> toCBOR sgNetworkMagic
        <> toCBOR sgNetworkId
        <> boundedRationalToCBOR sgActiveSlotsCoeff
        <> toCBOR sgSecurityParam
        <> toCBOR sgVestMultiple
        <> toCBOR (unEpochSize sgEpochLength)
        <> toCBOR sgSlotsPerKESPeriod
        <> toCBOR sgMaxKESEvolutions
        <> toCBOR sgSlotLength
        <> toCBOR sgUpdateQuorum
        <> toCBOR sgMaxEntropicSupply
        <> toCBOR sgProtocolParams
        <> mapToCBOR sgGenDelegs
        <> mapToCBOR sgVestedDelegs
        <> mapToCBOR sgInitialFunds
        <> toCBOR sgStaking

instance Era era => FromCBOR (SophieGenesis era) where
  fromCBOR = do
    decodeRecordNamed "SophieGenesis" (const 17) $ do
      sgSystemStart <- utcTimeFromCBOR
      sgNetworkMagic <- fromCBOR
      sgNetworkId <- fromCBOR
      sgActiveSlotsCoeff <- boundedRationalFromCBOR
      sgSecurityParam <- fromCBOR
      sgVestMultiple <- fromCBOR
      sgEpochLength <- fromCBOR
      sgSlotsPerKESPeriod <- fromCBOR
      sgMaxKESEvolutions <- fromCBOR
      sgSlotLength <- fromCBOR
      sgUpdateQuorum <- fromCBOR
      sgMaxEntropicSupply <- fromCBOR
      sgProtocolParams <- fromCBOR
      sgGenDelegs <- mapFromCBOR
      sgVestedDelegs <- mapFromCBOR
      sgInitialFunds <- mapFromCBOR
      sgStaking <- fromCBOR
      pure $
        SophieGenesis
          sgSystemStart
          sgNetworkMagic
          sgNetworkId
          sgActiveSlotsCoeff
          sgSecurityParam
          sgVestMultiple
          (EpochSize sgEpochLength)
          sgSlotsPerKESPeriod
          sgMaxKESEvolutions
          sgSlotLength
          sgUpdateQuorum
          sgMaxEntropicSupply
          sgProtocolParams
          sgGenDelegs
          sgVestedDelegs
          sgInitialFunds
          sgStaking

{-------------------------------------------------------------------------------
  Genesis UTxO
-------------------------------------------------------------------------------}

genesisUTxO ::
  forall era.
  (Era era, UsesTxOut era) =>
  SophieGenesis era ->
  UTxO era
genesisUTxO genesis =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
        | (addr, amount) <- Map.toList (sgInitialFunds genesis),
          let txIn = initialFundsPseudoTxIn addr
              txOut = makeTxOut (Proxy @era) addr (Val.inject amount)
      ]

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Sophie initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
initialFundsPseudoTxIn :: forall crypto. CC.Crypto crypto => Addr crypto -> TxIn crypto
initialFundsPseudoTxIn addr =
  TxIn (pseudoTxId addr) 0
  where
    pseudoTxId =
      TxId
        . unsafeMakeSafeHash
        . ( Crypto.castHash ::
              Crypto.Hash (HASH crypto) (Addr crypto) ->
              Crypto.Hash (HASH crypto) EraIndependentTxBody
          )
        . Crypto.hashWith serialiseAddr

{-------------------------------------------------------------------------------
  Genesis validation
-------------------------------------------------------------------------------}

data ValidationErr
  = EpochNotLongEnough EpochSize Word64 Rational EpochSize
  | MaxKESEvolutionsUnsupported Word64 Word
  | QuorumTooSmall Word64 Word64 Word64
  | SentryNotValid Word64 Word64 Word64
  deriving (Eq, Show)

describeValidationErr :: ValidationErr -> Text
describeValidationErr (EpochNotLongEnough es secParam asc minEpochSize) =
  mconcat
    [ "Epoch length is too low. Your epoch length of ",
      Text.pack (show es),
      " does not meet the minimum epoch length of ",
      Text.pack (show minEpochSize),
      " required by your choice of parameters for k and f: ",
      Text.pack (show secParam),
      " and ",
      Text.pack (show asc),
      ". Epochs should be at least 10k/f slots long."
    ]
describeValidationErr (MaxKESEvolutionsUnsupported reqKES supportedKES) =
  mconcat
    [ "You have specified a 'maxKESEvolutions' higher",
      " than that supported by the underlying algorithm.",
      " You requested ",
      Text.pack (show reqKES),
      " but the algorithm supports a maximum of ",
      Text.pack (show supportedKES)
    ]
describeValidationErr (QuorumTooSmall q maxTooSmal nodes) =
  mconcat
    [ "You have specified an 'updateQuorum' which is",
      " too small compared to the number of genesis nodes.",
      " You requested ",
      Text.pack (show q),
      ", but given ",
      Text.pack (show nodes),
      " genesis nodes 'updateQuorum' must be greater than ",
      Text.pack (show maxTooSmal)
    ]
describeValidationErr (SentryNotValid s sentryVerVal numSentry ) =
  mconcat
    [ "You have specified a 'vestMultiple' which is",
      " invalid compared to the sentry val verification.",
      " You requested ",
      Text.pack (show s),
      ", but given ",
      Text.pack (show numSentry),
      " vested holders, 'vestMultiple must be greater than",
      Text.pack (show sentryVerVal)
    ]

-- | Do some basic sanity checking on the Sophie genesis file. #TODO vested 777 check
validateGenesis ::
  forall era.
  Era era =>
  SophieGenesis era ->
  Either [ValidationErr] ()
validateGenesis
  SophieGenesis
    { sgEpochLength,
      sgActiveSlotsCoeff,
      sgMaxKESEvolutions,
      sgSecurityParam,
      sgVestMultiple,
      sgUpdateQuorum,
      sgGenDelegs,
      sgVestedDelegs
    } =
    case catMaybes errors of
      [] -> Right ()
      xs -> Left xs
    where
      errors =
        [ checkEpochLength,
          checkKesEvolutions,
          checkQuorumSize,
          checkSentryVal,
          checkVestedSize
        ]
      checkEpochLength =
        let activeSlotsCoeff = unboundRational sgActiveSlotsCoeff
            minLength =
              EpochSize . ceiling $
                fromIntegral @_ @Double (3 * sgSecurityParam)
                  / fromRational activeSlotsCoeff
         in if minLength > sgEpochLength
              then
                Just $
                  EpochNotLongEnough
                    sgEpochLength
                    sgSecurityParam
                    activeSlotsCoeff
                    minLength
              else Nothing
      checkKesEvolutions =
        if sgMaxKESEvolutions
          <= fromIntegral (totalPeriodsKES (Proxy @(KES (Crypto era))))
          then Nothing
          else
            Just $
              MaxKESEvolutionsUnsupported
                sgMaxKESEvolutions
                (totalPeriodsKES (Proxy @(KES (Crypto era))))
      checkQuorumSize =
        let numGenesisNodes = fromIntegral $ length sgGenDelegs
            maxTooSmal = numGenesisNodes `div` 2
         in if numGenesisNodes == 0 || sgUpdateQuorum > maxTooSmal
              then Nothing
              else Just $ QuorumTooSmall sgUpdateQuorum maxTooSmal numGenesisNodes
      checkSentryVal =
        let numSentry = fromIntegral $ length sgVestedDelegs `div` 3
            sentryVerVal = numSentry `div` 2
         in if numSentry == 0 || sgVestMultiple > sentryVerVal
              then Nothing 
              else Just $ SentryNotValid sgVestMultiple sentryVerVal numSentry
      checkVestedSize =
        let numVested = fromIntegral $ length sgVestedDelegs
            maxTooSmal = numVested `div` 2
         in if numVested == 0 || sgUpdateQuorum > maxTooSmal
              then Nothing
              else Just $ QuorumTooSmall sgUpdateQuorum maxTooSmal numVested
      

{-------------------------------------------------------------------------------
  Construct 'Globals' using 'SophieGenesis'
-------------------------------------------------------------------------------}

mkSophieGlobals ::
  SophieGenesis era ->
  EpochInfo (Either Text) ->
  Natural ->
  Globals
mkSophieGlobals genesis epochInfoAc maxMajorPV =
  Globals
    { activeSlotCoeff = sgActiveSlotCoeff genesis,
      epochInfoWithErr = epochInfoAc,
      maxKESEvo = sgMaxKESEvolutions genesis,
      maxEntropicSupply = sgMaxEntropicSupply genesis,
      maxMajorPV = maxMajorPV,
      networkId = sgNetworkId genesis,
      quorum = sgUpdateQuorum genesis,
      randomnessStabilisationWindow,
      securityParameter = k,
      vestMultiple = sgVestMultiple genesis,
      slotsPerKESPeriod = sgSlotsPerKESPeriod genesis,
      stabilityWindow,
      systemStart
    }
  where
    systemStart = SystemStart $ sgSystemStart genesis
    k = sgSecurityParam genesis
    stabilityWindow =
      computeStabilityWindow k (sgActiveSlotCoeff genesis)
    randomnessStabilisationWindow =
      computeRandomnessStabilisationWindow k (sgActiveSlotCoeff genesis)
