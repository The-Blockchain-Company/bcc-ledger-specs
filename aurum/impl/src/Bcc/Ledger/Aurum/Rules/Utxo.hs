{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Ledger.Aurum.Rules.Utxo where

import Bcc.Binary (FromCBOR (..), ToCBOR (..), serialize)
import Bcc.Ledger.Address
  ( Addr (..),
    RewardAcnt,
    bootstrapAddressAttrsSize,
    getNetwork,
    getRwdNetwork,
  )
import Bcc.Ledger.Aurum.Data (dataHashSize)
import Bcc.Ledger.Aurum.Rules.Utxos (UTXOS, UtxosPredicateFailure)
import Bcc.Ledger.Aurum.Scripts (ExUnits (..), Prices, pointWiseExUnits)
import Bcc.Ledger.Aurum.Tx (ValidatedTx (..), minfee, totExUnits)
import qualified Bcc.Ledger.Aurum.Tx as Aurum (ValidatedTx)
import Bcc.Ledger.Aurum.TxBody
  ( TxOut (..),
    txnetworkid',
  )
import qualified Bcc.Ledger.Aurum.TxBody as Aurum (TxBody, TxOut)
import qualified Bcc.Ledger.Aurum.TxSeq as Aurum (TxSeq)
import Bcc.Ledger.Aurum.TxWitness (TxWitness (txrdmrs'), nullRedeemers)
import Bcc.Ledger.BaseTypes
  ( Network,
    SophieBase,
    StrictMaybe (..),
    epochInfoWithErr,
    networkId,
    systemStart,
  )
import Bcc.Ledger.Coin
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Credential (Credential (..))
import Bcc.Ledger.Era (Crypto, Era, ValidateScript (..))
import qualified Bcc.Ledger.Era as Era
import qualified Bcc.Ledger.Jen.Value as Aurum (Value)
import Bcc.Ledger.Rules.ValidationMode ((?!#))
import Bcc.Ledger.Sophie.Constraints
  ( UsesPParams,
  )
import Bcc.Ledger.SophieMA.Rules.Utxo (consumed)
import Bcc.Ledger.SophieMA.Timelocks (ValidityInterval (..), inInterval)
import qualified Bcc.Ledger.Val as Val
import Bcc.Slotting.EpochInfo.API (epochInfoSlotToUTCTime)
import Bcc.Slotting.Slot (SlotNo)
import Control.Iterate.SetAlgebra (dom, eval, (⊆), (◁), (➖))
import Control.Monad (unless)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Coders
  ( Decode (..),
    Encode (..),
    Wrapped (Open),
    decode,
    decodeList,
    decodeSet,
    encode,
    encodeFoldable,
    (!>),
    (<!),
  )
import Data.Coerce (coerce)
import Data.Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import qualified Sophie.Spec.Ledger.LedgerState as Sophie
import qualified Sophie.Spec.Ledger.STS.Utxo as Sophie
import Sophie.Spec.Ledger.Tx (TxIn)
import Sophie.Spec.Ledger.TxBody (unWdrl)
import Sophie.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    txouts,
    unUTxO,
  )

-- | Compute an estimate of the size of storing one UTxO entry.
-- This function implements the UTxO entry size estimate done by scaledMinDeposit in the SophieMA era
utxoEntrySize :: Era era => TxOut era -> Integer
utxoEntrySize txout = utxoEntrySizeWithoutVal + Val.size v + dataHashSize dh
  where
    v = getField @"value" txout
    dh = getField @"datahash" txout
    -- lengths obtained from tracing on HeapWords of inputs and outputs
    -- obtained experimentally, and number used here
    -- units are Word64s

    -- size of UTxO entry excluding the Value part
    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 27 -- 6 + txoutLenNoVal [14] + txinLen [7]

-- ============================================

-- | The uninhabited type that marks the Aurum UTxO rule
data AurumUTXO era

-- ==========================================================

data UtxoPredicateFailure era
  = -- | The bad transaction inputs
    BadInputsUTxO
      !(Set (TxIn (Crypto era)))
  | OutsideValidityIntervalUTxO
      !ValidityInterval
      -- ^ transaction's validity interval
      !SlotNo
      -- ^ current slot
  | MaxTxSizeUTxO
      !Integer
      -- ^ the actual transaction size
      !Integer
      -- ^ the max transaction size
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      !Coin
      -- ^ the minimum fee for this transaction
      !Coin
      -- ^ the fee supplied in this transaction
  | ValueNotConservedUTxO
      !(Core.Value era)
      -- ^ the Coin consumed by this transaction
      !(Core.Value era)
      -- ^ the Coin produced by this transaction
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      !Network -- the expected network id
      !(Set (Addr (Crypto era)))
  | WrongNetworkWithdrawal
      !Network
      -- ^ the expected network id
      !(Set (RewardAcnt (Crypto era)))
      -- ^ the set of reward addresses with incorrect network IDs
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO
      ![Core.TxOut era]
  | -- | Subtransition Failures
    UtxosFailure (PredicateFailure (Core.EraRule "UTXOS" era))
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      ![Core.TxOut era]
  | TriesToForgeBCC
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      ![(Int, Int, Core.TxOut era)]
  | InsufficientCollateral
      !Coin
      -- ^ balance computed
      !Coin
      -- ^ the required collateral for the given fee
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO
      !(UTxO era)
  | ExUnitsTooBigUTxO
      !ExUnits
      -- ^ Max EXUnits from the protocol parameters
      !ExUnits
      -- ^ EXUnits supplied
  | -- | The inputs marked for use as fees contain non-BCC tokens
    CollateralContainsNonBCC !(Core.Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      !Network -- Actual Network ID
      !Network -- Network ID in transaction body
  | OutsideForecast
      !SlotNo -- slot number outside consensus forecast range
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      !Natural -- Max allowed collateral inputs
      !Natural -- Number of collateral inputs
  | NoCollateralInputs
  deriving (Generic)

deriving stock instance
  ( Era era,
    Show (Core.Value era),
    Show (Core.TxOut era),
    Show (Core.TxBody era),
    Show (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Show (UtxoPredicateFailure era)

deriving stock instance
  ( Eq (Core.Value era),
    Eq (Core.TxOut era),
    Eq (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Eq (UtxoPredicateFailure era)

instance
  ( Era era,
    Sophie.TransUTxOState NoThunks era,
    NoThunks (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  NoThunks (UtxoPredicateFailure era)

newtype UtxoEvent era
  = UtxosEvent (Event (Core.EraRule "UTXOS" era))

-- | Returns true for VKey locked addresses, and false for any kind of
-- script-locked address.
isKeyHashAddr :: Addr crypto -> Bool
isKeyHashAddr (AddrBootstrap _) = True
isKeyHashAddr (Addr _ (KeyHashObj _) _) = True
isKeyHashAddr _ = False

vKeyLocked :: Era era => TxOut era -> Bool
vKeyLocked txout = isKeyHashAddr (getField @"address" txout)

-- | feesOK is a predicate with several parts. Some parts only apply in special circumstances.
--   1) The fee paid is >= the minimum fee
--   2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
--   3) The collateral consists only of VKey addresses
--   4) The collateral is sufficient to cover the appropriate percentage of the
--      fee marked in the transaction
--   5) The collateral inputs do not contain any non-BCC part
--   6) There is at least one collateral input
--   As a TransitionRule it will return (), and raise an error (rather than
--   return) if any of the required parts are False.
feesOK ::
  forall era.
  ( Era era,
    ValidateScript era, -- isTwoPhaseScriptAddress
    Core.TxOut era ~ Aurum.TxOut era, -- balance requires this,
    Core.Tx era ~ Aurum.ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    HasField
      "collateral" -- to get inputs to pay the fees
      (Core.TxBody era)
      (Set (TxIn (Crypto era))),
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "_collateralPercentage" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Rule (AurumUTXO era) 'Transition ()
feesOK pp tx (UTxO m) = do
  let txb = getField @"body" tx
      theFee = getField @"txfee" txb -- Coin supplied to pay fees
      collateral = getField @"collateral" txb -- Inputs allocated to pay theFee
      utxoCollateral = eval (collateral ◁ m) -- restrict Utxo to those inputs we use to pay fees.
      bal = balance @era (UTxO utxoCollateral)
      minimumFee = minfee @era pp tx
      collPerc = getField @"_collateralPercentage" pp
  -- Part 1
  (minimumFee <= theFee) ?! FeeTooSmallUTxO minimumFee theFee
  -- Part 2
  unless (nullRedeemers . txrdmrs' . wits $ tx) $ do
    -- Part 3
    all vKeyLocked utxoCollateral
      ?! ScriptsNotPaidUTxO
        (UTxO (Map.filter (not . vKeyLocked) utxoCollateral))
    -- Part 4
    (Val.scale (100 :: Natural) (Val.coin bal) >= Val.scale collPerc theFee)
      ?! InsufficientCollateral
        (Val.coin bal)
        (rationalToCoinViaCeiling $ (fromIntegral collPerc * unCoin theFee) % 100)
    -- Part 5
    Val.inject (Val.coin bal) == bal ?! CollateralContainsNonBCC bal
    -- Part 6
    not (null utxoCollateral) ?! NoCollateralInputs

-- ================================================================

-- | The UTxO transition rule for the Aurum eras.
utxoTransition ::
  forall era.
  ( Era era,
    ValidateScript era,
    -- instructions for calling UTXOS from AurumUTXO
    Embed (Core.EraRule "UTXOS" era) (AurumUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Sophie.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Sophie.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    -- We leave Core.PParams abstract
    UsesPParams era,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_coinsPerUTxOWord" (Core.PParams era) Coin,
    HasField "_maxValSize" (Core.PParams era) Natural,
    HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "_maxCollateralInputs" (Core.PParams era) Natural,
    -- We fix Core.Tx, Core.Value, Core.TxBody, and Core.TxOut
    Core.Tx era ~ Aurum.ValidatedTx era,
    Core.TxOut era ~ Aurum.TxOut era,
    Core.Value era ~ Aurum.Value (Crypto era),
    Core.TxBody era ~ Aurum.TxBody era,
    Core.Witnesses era ~ TxWitness era,
    Era.TxSeq era ~ Aurum.TxSeq era
  ) =>
  TransitionRule (AurumUTXO era)
utxoTransition = do
  TRC (Sophie.UtxoEnv slot pp stakepools _genDelegs, u, tx) <- judgmentContext
  let Sophie.UTxOState utxo _deposits _fees _ppup = u

  {-   txb := txbody tx   -}
  {-   (,i_f) := txvldttx   -}
  let txb = body tx
      vi@(ValidityInterval _ i_f) = getField @"vldt" txb
      inputsAndCollateral =
        Set.union
          (getField @"inputs" txb)
          (getField @"collateral" txb)

  {-   ininterval slot (txvldt tx)    -}
  inInterval slot vi
    ?! OutsideValidityIntervalUTxO (getField @"vldt" txb) slot

  {-   epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇   -}
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfoWithErr
  case i_f of
    SNothing -> pure ()
    SJust ifj -> case epochInfoSlotToUTCTime ei sysSt ifj of
      -- if tx has non-native scripts, end of validity interval must translate to time
      Left _ -> (nullRedeemers . txrdmrs' . wits $ tx) ?! OutsideForecast ifj
      Right _ -> pure ()

  {-   txins txb ≠ ∅   -}
  not (Set.null (getField @"inputs" txb)) ?!# InputSetEmptyUTxO

  {-   feesOKp p tx utxo   -}
  feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {-   (txins txb) ∪ (collateral txb)  ⊆ dom utxo   -}
  eval (inputsAndCollateral ⊆ dom utxo)
    ?! BadInputsUTxO (eval (inputsAndCollateral ➖ dom utxo))

  {-   consumedpp utxo txb = producedpp poolParams txb    -}
  let consumed_ = consumed @era pp utxo txb
      produced_ = Sophie.produced @era pp (`Map.notMember` stakepools) txb
  consumed_ == produced_ ?! ValueNotConservedUTxO consumed_ produced_

  {-   bccID  ∉ supp mint tx   -}
  -- Check that the mint field does not try to mint BCC.
  -- Here in the implementation, we store the bccId policyID in the coin field of the value.
  Val.coin (getField @"mint" txb) == Val.zero ?!# TriesToForgeBCC

  {-   ∀ txout ∈ txouts txb, getValuetxout ≥ inject(uxoEntrySizetxout ∗ coinsPerUTxOWord p)   -}
  let (Coin coinsPerUTxOWord) = getField @"_coinsPerUTxOWord" pp
      outputs = Map.elems $ unUTxO (txouts txb)
      outputsTooSmall =
        filter
          ( \out ->
              let v = getField @"value" out
               in not $
                    Val.pointwise -- pointwise is used because non-bcc amounts must be >= 0 too
                      (>=)
                      v
                      (Val.inject $ Coin (utxoEntrySize out * coinsPerUTxOWord))
          )
          outputs
  null outputsTooSmall ?! OutputTooSmallUTxO outputsTooSmall

  {-   ∀ txout ∈ txouts txb, serSize(getValuetxout) ≤ (maxTxSizep p)∗(maxValSizep pp)   -}
  -- use serialized length of Value because this Value size is being limited inside a serialized Tx
  let maxValSize = getField @"_maxValSize" pp
      outputsTooBig = foldl' accum [] outputs
        where
          accum ans out =
            let v = getField @"value" out
                sersize = (fromIntegral . BSL.length . serialize) v
             in if sersize > maxValSize
                  then (fromIntegral sersize, fromIntegral maxValSize, out) : ans
                  else ans
  null outputsTooBig ?! OutputTooBigUTxO outputsTooBig

  {-    ∀ ( _ ↦ (a,_)) ∈ txoutstxb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64   -}
  -- Bootstrap (i.e. Cole) addresses have variable sized attributes in them.
  -- It is important to limit their overall size.
  let outputsAttrsTooBig =
        filter
          ( \out -> case getField @"address" out of
              AddrBootstrap addr -> bootstrapAddressAttrsSize addr > 64
              _ -> False
          )
          outputs
  null outputsAttrsTooBig ?!# OutputBootAddrAttrsTooBig outputsAttrsTooBig

  {-   ∀(_ ↦ (a,_)) ∈ txouts txb, netId a = NetworkId   -}
  ni <- liftSTS $ asks networkId
  let addrsWrongNetwork =
        filter
          (\a -> getNetwork a /= ni)
          (fmap (getField @"address") $ toList $ getField @"outputs" txb)
  null addrsWrongNetwork ?!# WrongNetwork ni (Set.fromList addrsWrongNetwork)

  {-   ∀ (a ↦ _) ∈ txwdrls txb, netId a = NetworkId   -}
  let wdrlsWrongNetwork =
        filter
          (\a -> getRwdNetwork a /= ni)
          (Map.keys . unWdrl . getField @"wdrls" $ txb)
  null wdrlsWrongNetwork
    ?!# WrongNetworkWithdrawal
      ni
      (Set.fromList wdrlsWrongNetwork)

  {-   txnetworkid txb = NetworkId   -}
  case txnetworkid' txb of
    SNothing -> pure ()
    SJust bid -> ni == bid ?!# WrongNetworkInTxBody ni bid

  {-   txsize tx ≤ maxTxSize pp   -}
  let maxTxSize_ = fromIntegral (getField @"_maxTxSize" pp)
      txSize_ = getField @"txsize" tx
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  {-   totExunits tx ≤ maxTxExUnits pp    -}
  let maxTxEx = getField @"_maxTxExUnits" pp
      totExunits = totExUnits tx -- This sums up the ExUnits for all embedded Zerepoch Scripts anywhere in the transaction.
  pointWiseExUnits (<=) totExunits maxTxEx ?! ExUnitsTooBigUTxO maxTxEx totExunits

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}
  let maxColl = getField @"_maxCollateralInputs" pp
      numColl = fromIntegral . Set.size $ getField @"collateral" txb
  numColl <= maxColl ?! TooManyCollateralInputs maxColl numColl

  trans @(Core.EraRule "UTXOS" era) =<< coerce <$> judgmentContext

--------------------------------------------------------------------------------
-- AurumUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( ValidateScript era,
    -- Instructions needed to call the UTXOS transition from this one.
    Embed (Core.EraRule "UTXOS" era) (AurumUTXO era),
    Environment (Core.EraRule "UTXOS" era) ~ Sophie.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Sophie.UTxOState era,
    Signal (Core.EraRule "UTXOS" era) ~ ValidatedTx era,
    -- We leave Core.PParams abstract
    UsesPParams era,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_maxTxSize" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_coinsPerUTxOWord" (Core.PParams era) Coin,
    HasField "_maxValSize" (Core.PParams era) Natural,
    HasField "_collateralPercentage" (Core.PParams era) Natural,
    HasField "_maxCollateralInputs" (Core.PParams era) Natural,
    -- We fix Core.Value, Core.TxBody, and Core.TxOut
    Core.Value era ~ Aurum.Value (Crypto era),
    Core.TxBody era ~ Aurum.TxBody era,
    Core.Witnesses era ~ TxWitness era,
    Core.TxOut era ~ Aurum.TxOut era,
    Era.TxSeq era ~ Aurum.TxSeq era,
    Core.Tx era ~ Aurum.ValidatedTx era
  ) =>
  STS (AurumUTXO era)
  where
  type State (AurumUTXO era) = Sophie.UTxOState era
  type Signal (AurumUTXO era) = ValidatedTx era
  type Environment (AurumUTXO era) = Sophie.UtxoEnv era
  type BaseM (AurumUTXO era) = SophieBase
  type PredicateFailure (AurumUTXO era) = UtxoPredicateFailure era
  type Event (AurumUTXO era) = UtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

instance
  ( Era era,
    STS (UTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ UtxosPredicateFailure era,
    Event (Core.EraRule "UTXOS" era) ~ Event (UTXOS era)
  ) =>
  Embed (UTXOS era) (AurumUTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Typeable era,
    Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  ToCBOR (UtxoPredicateFailure era)
  where
  toCBOR x = encode (encFail x)

encFail ::
  forall era.
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  UtxoPredicateFailure era ->
  Encode 'Open (UtxoPredicateFailure era)
encFail (BadInputsUTxO ins) =
  Sum (BadInputsUTxO @era) 0 !> E encodeFoldable ins
encFail (OutsideValidityIntervalUTxO a b) =
  Sum OutsideValidityIntervalUTxO 1 !> To a !> To b
encFail (MaxTxSizeUTxO a b) =
  Sum MaxTxSizeUTxO 2 !> To a !> To b
encFail InputSetEmptyUTxO =
  Sum InputSetEmptyUTxO 3
encFail (FeeTooSmallUTxO a b) =
  Sum FeeTooSmallUTxO 4 !> To a !> To b
encFail (ValueNotConservedUTxO a b) =
  Sum (ValueNotConservedUTxO @era) 5 !> To a !> To b
encFail (OutputTooSmallUTxO outs) =
  Sum (OutputTooSmallUTxO @era) 6 !> E encodeFoldable outs
encFail (UtxosFailure a) =
  Sum (UtxosFailure @era) 7 !> To a
encFail (WrongNetwork right wrongs) =
  Sum (WrongNetwork @era) 8 !> To right !> E encodeFoldable wrongs
encFail (WrongNetworkWithdrawal right wrongs) =
  Sum (WrongNetworkWithdrawal @era) 9 !> To right !> E encodeFoldable wrongs
encFail (OutputBootAddrAttrsTooBig outs) =
  Sum (OutputBootAddrAttrsTooBig @era) 10 !> E encodeFoldable outs
encFail TriesToForgeBCC =
  Sum TriesToForgeBCC 11
encFail (OutputTooBigUTxO outs) =
  Sum (OutputTooBigUTxO @era) 12 !> E encodeFoldable outs
encFail (InsufficientCollateral a b) =
  Sum InsufficientCollateral 13 !> To a !> To b
encFail (ScriptsNotPaidUTxO a) =
  Sum ScriptsNotPaidUTxO 14 !> To a
encFail (ExUnitsTooBigUTxO a b) =
  Sum ExUnitsTooBigUTxO 15 !> To a !> To b
encFail (CollateralContainsNonBCC a) =
  Sum CollateralContainsNonBCC 16 !> To a
encFail (WrongNetworkInTxBody a b) =
  Sum WrongNetworkInTxBody 17 !> To a !> To b
encFail (OutsideForecast a) =
  Sum OutsideForecast 18 !> To a
encFail (TooManyCollateralInputs a b) =
  Sum TooManyCollateralInputs 19 !> To a !> To b
encFail NoCollateralInputs =
  Sum NoCollateralInputs 20

decFail ::
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  Word ->
  Decode 'Open (UtxoPredicateFailure era)
decFail 0 = SumD BadInputsUTxO <! D (decodeSet fromCBOR)
decFail 1 = SumD OutsideValidityIntervalUTxO <! From <! From
decFail 2 = SumD MaxTxSizeUTxO <! From <! From
decFail 3 = SumD InputSetEmptyUTxO
decFail 4 = SumD FeeTooSmallUTxO <! From <! From
decFail 5 = SumD ValueNotConservedUTxO <! From <! From
decFail 6 = SumD OutputTooSmallUTxO <! D (decodeList fromCBOR)
decFail 7 = SumD UtxosFailure <! From
decFail 8 = SumD WrongNetwork <! From <! D (decodeSet fromCBOR)
decFail 9 = SumD WrongNetworkWithdrawal <! From <! D (decodeSet fromCBOR)
decFail 10 = SumD OutputBootAddrAttrsTooBig <! D (decodeList fromCBOR)
decFail 11 = SumD TriesToForgeBCC
decFail 12 = SumD OutputTooBigUTxO <! D (decodeList fromCBOR)
decFail 13 = SumD InsufficientCollateral <! From <! From
decFail 14 = SumD ScriptsNotPaidUTxO <! From
decFail 15 = SumD ExUnitsTooBigUTxO <! From <! From
decFail 16 = SumD CollateralContainsNonBCC <! From
decFail 17 = SumD WrongNetworkInTxBody <! From <! From
decFail 18 = SumD OutsideForecast <! From
decFail 19 = SumD TooManyCollateralInputs <! From <! From
decFail 20 = SumD NoCollateralInputs
decFail n = Invalid n

instance
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.Value era),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOS" era))
  ) =>
  FromCBOR (UtxoPredicateFailure era)
  where
  fromCBOR = decode (Summands "UtxoPredicateFailure" decFail)
