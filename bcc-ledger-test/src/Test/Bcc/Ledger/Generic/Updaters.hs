{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Bcc.Ledger.Generic.Updaters where

import Bcc.Crypto.DSIGN.Class ()
import Bcc.Ledger.Address (Addr (..))
import Bcc.Ledger.Evie (EvieEra)
import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data (AuxiliaryDataHash, Data (..), DataHash, hashData)
import Bcc.Ledger.Aurum.Language (Language (..))
import qualified Bcc.Ledger.Aurum.PParams as Aurum (PParams' (..))
import Bcc.Ledger.Aurum.Scripts
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    alwaysFails,
    alwaysSucceeds,
  )
import Bcc.Ledger.Aurum.Tx (hashScriptIntegrity)
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxBody as Aurum (TxOut (..))
import Bcc.Ledger.Aurum.TxWitness (Redeemers (..), TxDats (..), TxWitness (..), unTxDats)
import Bcc.Ledger.BaseTypes
  ( Network (..),
    NonNegativeInterval,
    Nonce,
    StrictMaybe (..),
    UnitInterval,
  )
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era (Era (..), ValidateScript (..))
import Bcc.Ledger.Hashes (ScriptHash (..))
import Bcc.Ledger.Keys
import Bcc.Ledger.Jen (JenEra)
import qualified Bcc.Ledger.Jen.Value as Jen (AssetName (..), PolicyID (..), Value (..))
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.SophieMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Bcc.Ledger.SophieMA.TxBody as MA (TxBody (..))
import Bcc.Ledger.Val (inject, (<+>))
import Bcc.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Data.ByteString.Char8 as BS
import Data.Default.Class (def)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq (empty, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Sophie.Spec.Ledger.Address.Bootstrap (BootstrapWitness (..))
import Sophie.Spec.Ledger.PParams (ProtVer (..))
import qualified Sophie.Spec.Ledger.PParams as PP (PParams, PParams' (..), Update)
import qualified Sophie.Spec.Ledger.Scripts as Multi
import Sophie.Spec.Ledger.Tx as Sophie (WitnessSetHKD (addrWits, bootWits, scriptWits), pattern WitnessSet)
import qualified Sophie.Spec.Ledger.Tx as Sophie (Tx (..))
import Sophie.Spec.Ledger.TxBody (DCert (..), TxIn (..), Wdrl (..), WitVKey (..))
import qualified Sophie.Spec.Ledger.TxBody as Sophie (TxBody (..), TxOut (..))
import Test.Bcc.Ledger.Generic.Indexed
import Test.Bcc.Ledger.Generic.Proof

-- =============================================
-- Making era parameterized Scripts

class (Era era, ValidateScript era) => Scriptic era where
  always :: Natural -> Proof era -> (Core.Script era)
  never :: Natural -> Proof era -> (Core.Script era)
  require :: KeyHash 'Witness (Crypto era) -> Proof era -> (Core.Script era)
  allOf :: [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)
  anyOf :: [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)
  mOf :: Int -> [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)

class Scriptic era => PostSophie era where
  before :: Int -> Proof era -> Core.Script era
  after :: Int -> Proof era -> Core.Script era

class HasTokens era where
  forge :: Integer -> Core.Script era -> Core.Value era

instance CC.Crypto c => Scriptic (SophieEra c) where
  never _ (Sophie _) = Multi.RequireAnyOf mempty -- always False
  always _ (Sophie _) = Multi.RequireAllOf mempty -- always True
  require key (Sophie _) = Multi.RequireSignature key
  allOf xs (Sophie c) = (Multi.RequireAllOf (map ($ Sophie c) xs))
  anyOf xs (Sophie c) = (Multi.RequireAnyOf (map ($ Sophie c) xs))
  mOf n xs (Sophie c) = (Multi.RequireMOf n (map ($ Sophie c) xs))

-- Make Scripts in EvieEra

instance CC.Crypto c => Scriptic (EvieEra c) where
  never _ (Evie _) = RequireAnyOf mempty -- always False
  always _ (Evie _) = RequireAllOf mempty -- always True
  require key (Evie _) = RequireSignature key
  allOf xs (Evie c) = (RequireAllOf (Seq.fromList (map ($ Evie c) xs)))
  anyOf xs (Evie c) = (RequireAnyOf (Seq.fromList (map ($ Evie c) xs)))
  mOf n xs (Evie c) = (RequireMOf n (Seq.fromList (map ($ Evie c) xs)))

instance CC.Crypto c => PostSophie (EvieEra c) where
  before n (Evie _) = RequireTimeStart (unique @SlotNo n)
  after n (Evie _) = RequireTimeExpire (unique @SlotNo n)

-- Make Scripts in Jen era

instance CC.Crypto c => Scriptic (JenEra c) where
  never _ (Jen _) = RequireAnyOf mempty -- always False
  always _ (Jen _) = RequireAllOf mempty -- always True
  require key (Jen _) = RequireSignature key
  allOf xs (Jen c) = (RequireAllOf (Seq.fromList (map ($ Jen c) xs)))
  anyOf xs (Jen c) = (RequireAnyOf (Seq.fromList (map ($ Jen c) xs)))
  mOf n xs (Jen c) = (RequireMOf n (Seq.fromList (map ($ Jen c) xs)))

instance CC.Crypto c => PostSophie (JenEra c) where
  before n (Jen _) = RequireTimeStart (unique @SlotNo n)
  after n (Jen _) = RequireTimeExpire (unique @SlotNo n)

instance forall c. CC.Crypto c => HasTokens (JenEra c) where
  forge n s = Jen.Value 0 $ Map.singleton pid (Map.singleton an n)
    where
      pid = Jen.PolicyID (hashScript @(JenEra c) s)
      an = Jen.AssetName $ BS.pack "an"

instance forall c. CC.Crypto c => HasTokens (AurumEra c) where
  forge n s = Jen.Value 0 $ Map.singleton pid (Map.singleton an n)
    where
      pid = Jen.PolicyID (hashScript @(AurumEra c) s)
      an = Jen.AssetName $ BS.pack "an"

-- Make Scripts in Aurum era

-- | Not every Aurum Script can be used in a Timelock context.
unTime :: CC.Crypto (Crypto era) => Proof era -> (Proof era -> Script era) -> Timelock (Crypto era)
unTime wit f = case f wit of
  (TimelockScript x) -> x
  (ZerepochScript "\SOH\NUL\NUL \ACK\SOH") -> (RequireAnyOf mempty)
  (ZerepochScript "\SOH\NUL\NUL \STX\NUL\NUL\DC1") -> (RequireAllOf mempty)
  (ZerepochScript _) -> error ("Zerepoch script in Timelock context")

instance CC.Crypto c => Scriptic (AurumEra c) where
  never n (Aurum _) = alwaysFails n -- always False
  always n (Aurum _) = alwaysSucceeds n -- always True
  require key (Aurum _) = TimelockScript (RequireSignature key)
  allOf xs (Aurum c) = TimelockScript (RequireAllOf (Seq.fromList (map (unTime (Aurum c)) xs)))
  anyOf xs (Aurum c) = TimelockScript (RequireAnyOf (Seq.fromList (map (unTime (Aurum c)) xs)))
  mOf n xs (Aurum c) = TimelockScript (RequireMOf n (Seq.fromList (map (unTime (Aurum c)) xs)))

instance CC.Crypto c => PostSophie (AurumEra c) where
  before n (Aurum _) = TimelockScript $ RequireTimeStart (unique @SlotNo n)
  after n (Aurum _) = TimelockScript $ RequireTimeExpire (unique @SlotNo n)

-- Some examples that work in multiple Eras
matchkey :: Scriptic era => Int -> Proof era -> Core.Script era
matchkey n era = require (theKeyHash n) era

test21 :: Scriptic era => Proof era -> Core.Script era
test21 wit = allOf [always 1, matchkey 1, anyOf [matchkey 2, matchkey 3]] $ wit

test22 :: PostSophie era => Proof era -> Core.Script era
test22 wit = mOf 2 [matchkey 1, before 100, anyOf [matchkey 2, matchkey 3]] $ wit

-- =========================================================================
-- Era parametric "empty" or initial values.

initVI :: ValidityInterval
initVI = ValidityInterval SNothing SNothing

initWdrl :: Wdrl crypto
initWdrl = Wdrl Map.empty

initValue :: Jen.Value crypto
initValue = (Jen.Value 0 Map.empty)

initialTxBody :: Era era => Proof era -> Core.TxBody era
initialTxBody (Sophie _) = Sophie.TxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) (SlotNo 0) SNothing SNothing
initialTxBody (Evie _) = MA.TxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) initVI SNothing SNothing (Coin 0)
initialTxBody (Jen _) = MA.TxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) initVI SNothing SNothing initValue
initialTxBody (Aurum _) =
  Aurum.TxBody
    Set.empty
    Set.empty
    Seq.empty
    Seq.empty
    initWdrl
    (Coin 0)
    initVI
    SNothing
    Set.empty
    initValue
    SNothing
    SNothing
    SNothing

initialWitnesses :: Era era => Proof era -> Core.Witnesses era
initialWitnesses (Sophie _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Evie _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Jen _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Aurum _) = TxWitness mempty mempty mempty mempty (Redeemers mempty)

initialTx :: forall era. Proof era -> Core.Tx era
initialTx era@(Sophie _) = Sophie.Tx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Evie _) = Sophie.Tx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Jen _) = Sophie.Tx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Aurum _) =
  Aurum.ValidatedTx
    (initialTxBody era)
    (initialWitnesses era)
    (Aurum.IsValid True)
    SNothing

initialPParams :: forall era. Proof era -> Core.PParams era
initialPParams (Sophie _) = def
initialPParams (Evie _) = def
initialPParams (Jen _) = def
initialPParams (Aurum _) = def

-- ===========================================================================
-- Upaters and the use of Policy to specify Merge Semantics and use of [t] as inputs.
-- When using the Updaters, one will usually consruct the fields by hand.
-- So if a Field consists of (Set t), (StrictSeq t), [t], (Maybe t), (StrictMaybe t), or (Map key t)
-- we will use a list, and convert to the appropriate type for each Field and Era.
-- Several of these: (Map key t), (Maybe t) and (StrictMaybe t) can be problematic
-- since they only have a well defined Merge semantics when (SemiGroup t) .
-- So we define specialized functions applyMap, applyMaybe and applySMaybe that raise
-- an error if a Merge semantics finds more than one copy of the elements being combined.
-- Users may choose what merge semantics they want by passing the right Policy
-- =============================================================================

data Policy
  = -- | Combine old and new values using Semigroup semantics (or raise an error if (Semgroup t) doesn't hold).
    Merge
  | -- | Always use the new value
    Override
  | -- | Combine old and new, but don't add any new values if they are already in old.
    NoDups

-- | filter out elements of 'xs' that are in 't'
nodups :: (Foldable t, Eq x) => t x -> [x] -> [x]
nodups t xs = filter (not . (`elem` t)) xs

class Merge t x | t -> x where
  merge :: t -> [x] -> t
  merge t xs = applyMerge Merge t xs
  applyMerge :: Policy -> t -> [x] -> t

instance (Show x, Eq x, Semigroup x) => Merge (Maybe x) x where
  applyMerge Merge Nothing [x] = Just x
  applyMerge Merge Nothing [] = Nothing
  applyMerge Merge (Just x) [y] = Just (x <> y)
  applyMerge Merge (Just x) [] = Just x
  applyMerge Override Nothing [x] = Just x
  applyMerge Override Nothing [] = Nothing
  applyMerge Override (Just _) [y] = Just y
  applyMerge Override (Just x) [] = Just x
  applyMerge NoDups Nothing [x] = Just x
  applyMerge NoDups Nothing [] = Nothing
  applyMerge NoDups (Just x) [y] = Just (if x == y then x else y)
  applyMerge NoDups (Just x) [] = Just x
  applyMerge _ _ xs =
    error ("Only null lists or lists with 1 element can be used in applyMerge for Maybe: " ++ show xs)

instance (Show x, Eq x, Semigroup x) => Merge (StrictMaybe x) x where
  applyMerge Merge SNothing [x] = SJust x
  applyMerge Merge SNothing [] = SNothing
  applyMerge Merge (SJust x) [y] = SJust (x <> y)
  applyMerge Merge (SJust x) [] = SJust x
  applyMerge Override SNothing [x] = SJust x
  applyMerge Override SNothing [] = SNothing
  applyMerge Override (SJust _) [y] = SJust y
  applyMerge Override (SJust x) [] = SJust x
  applyMerge NoDups SNothing [x] = SJust x
  applyMerge NoDups SNothing [] = SNothing
  applyMerge NoDups (SJust x) [y] = SJust (if x == y then x else y)
  applyMerge NoDups (SJust x) [] = SJust x
  applyMerge _ _ xs =
    error ("Only null lists or lists with 1 element can be used in applyMerge for Maybe: " ++ show xs)

instance Ord x => Merge (Set x) x where
  applyMerge Merge set ts = Set.union set (Set.fromList ts)
  applyMerge Override _set ts = (Set.fromList ts)
  applyMerge NoDups set ts = Set.union set (Set.fromList (nodups set ts))

instance Eq x => Merge (StrictSeq x) x where
  applyMerge Override _seqx ts = (Seq.fromList ts)
  applyMerge Merge seqx ts = seqx <> (Seq.fromList ts)
  applyMerge NoDups seqx ts = seqx <> (Seq.fromList (nodups seqx ts))

instance Eq x => Merge [x] x where
  applyMerge Merge list ts = list ++ ts
  applyMerge Override _list ts = ts
  applyMerge NoDups list ts = list ++ (nodups list ts)

instance (Show t, Semigroup t, Show key, Ord key) => Merge (Map key t) (key, t) where
  applyMerge Override _m ts = (Map.fromList ts)
  applyMerge NoDups m ts = Map.union m (Map.fromList ts)
  applyMerge Merge m ts = Map.unionWith (<>) m (Map.fromList ts)

-- | Use this when the range of the map is not a Semigroup. Note Merge cas can fail.
applyMaybe :: Show a => Policy -> Maybe a -> [a] -> Maybe a
applyMap :: (Ord key, Show t, Show key) => String -> Policy -> Map key t -> [(key, t)] -> Map key t
applyMap _ Override _m pairs = Map.fromList pairs
applyMap _ NoDups m ts = Map.union m (Map.fromList ts)
applyMap message Merge m pairs = List.foldl' accum m pairs
  where
    accum ans (k, t) = Map.insertWithKey checkdisjoint k t ans
    checkdisjoint k a b =
      error
        ( "\nWhile merging maps with supposedly disjoint domains, stored in a field: " ++ message
            ++ "\n  We found a common domain element: "
            ++ show k
            ++ "\n  With values: "
            ++ show a
            ++ "  and  "
            ++ show b
        )

-- | Use this when the Maybe type does not have a Semigroup instance
applyMaybe Merge (Just x) [y] = error ("No Semigroup in applyMaybe: " ++ show x ++ "  " ++ show y)
applyMaybe _ Nothing [x] = Just x
applyMaybe _ Nothing [] = Nothing
applyMaybe _ (Just _) [y] = Just y
applyMaybe _ (Just x) [] = Just x
applyMaybe _ _ xs =
  error ("Only null lists or lists with 1 element can be used in applyMaybe: " ++ show xs)

-- | Use this when the StrictMaybe type does not have a Semigroup instance
applySMaybe :: Show a => Policy -> StrictMaybe a -> [a] -> StrictMaybe a
applySMaybe Merge (SJust x) [y] = error ("No Semigroup in applyStrictMaybe: " ++ show x ++ "  " ++ show y)
applySMaybe _ SNothing [x] = SJust x
applySMaybe _ SNothing [] = SNothing
applySMaybe _ (SJust _) [y] = SJust y
applySMaybe _ (SJust x) [] = SJust x
applySMaybe _ _ xs =
  error ("Only null lists or lists with 1 element can be used in applyStrictMaybe: " ++ show xs)

-- ====================================================================
-- Building Era parametric Records
-- ====================================================================

-- Updaters for Tx

data TxField era
  = Body (Core.TxBody era)
  | Body' [TxBodyField era]
  | Witnesses (Core.Witnesses era)
  | Witnesses' [WitnessesField era]
  | AuxData [(Core.AuxiliaryData era)] -- 0 or 1 element, represents Maybe type
  | Valid Bool

updateTx :: Policy -> Proof era -> Core.Tx era -> TxField era -> Core.Tx era
updateTx p (wit@(Sophie _)) (tx@(Sophie.Tx b w d)) dt =
  case dt of
    Body fbody -> Sophie.Tx fbody w d
    Body' bfields -> Sophie.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Sophie.Tx b fwit d
    Witnesses' wfields -> Sophie.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Sophie.Tx b w (applySMaybe p d faux)
    Valid _ -> tx
updateTx p (wit@(Evie _)) (tx@(Sophie.Tx b w d)) dt =
  case dt of
    Body fbody -> Sophie.Tx fbody w d
    Body' bfields -> Sophie.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Sophie.Tx b fwit d
    Witnesses' wfields -> Sophie.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Sophie.Tx b w (applySMaybe p d faux)
    Valid _ -> tx
updateTx p (wit@(Jen _)) (tx@(Sophie.Tx b w d)) dt =
  case dt of
    Body fbody -> Sophie.Tx fbody w d
    Body' bfields -> Sophie.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Sophie.Tx b fwit d
    Witnesses' wfields -> Sophie.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Sophie.Tx b w (applySMaybe p d faux)
    Valid _ -> tx
updateTx p wit@(Aurum _) (Aurum.ValidatedTx b w iv d) dt =
  case dt of
    Body fbody -> Aurum.ValidatedTx fbody w iv d
    Body' bfields -> Aurum.ValidatedTx (newTxBody p wit bfields) w iv d
    Witnesses fwit -> Aurum.ValidatedTx b fwit iv d
    Witnesses' wfields -> Aurum.ValidatedTx b (newWitnesses p wit wfields) iv d
    AuxData faux -> Aurum.ValidatedTx b w iv (applySMaybe p d faux)
    Valid iv' -> Aurum.ValidatedTx b w (Aurum.IsValid iv') d

newTx :: Policy -> Proof era -> [TxField era] -> Core.Tx era
newTx p era = List.foldl' (updateTx p era) (initialTx era)

--------------------------------------------------------------------
-- Updaters for TxBody

data TxBodyField era
  = Inputs [TxIn (Crypto era)]
  | Collateral [TxIn (Crypto era)]
  | Outputs [Core.TxOut era]
  | Certs [DCert (Crypto era)]
  | Wdrls (Wdrl (Crypto era))
  | Txfee Coin
  | Vldt ValidityInterval
  | Update [PP.Update era] -- 0 or 1 element, represents Maybe type
  | ReqSignerHashes [KeyHash 'Witness (Crypto era)]
  | Mint (Core.Value era)
  | WppHash [Aurum.ScriptIntegrityHash (Crypto era)] -- 0 or 1 element, represents Maybe type
  | AdHash [AuxiliaryDataHash (Crypto era)] -- 0 or 1 element, represents Maybe type
  | Txnetworkid (StrictMaybe Network)

updateTxBody :: Policy -> Proof era -> Core.TxBody era -> TxBodyField era -> Core.TxBody era
updateTxBody p (Sophie _) tx dt = case dt of
  (Inputs is) -> tx {Sophie._inputs = applyMerge p (Sophie._inputs tx) is}
  (Collateral is) -> tx {Sophie._inputs = applyMerge p (Sophie._inputs tx) is}
  (Outputs outs) -> tx {Sophie._outputs = applyMerge p (Sophie._outputs tx) outs}
  (Certs cs) -> tx {Sophie._certs = applyMerge p (Sophie._certs tx) cs}
  (Wdrls ws) -> tx {Sophie._wdrls = Wdrl (Map.unionWith (<+>) (unWdrl (Sophie._wdrls tx)) (unWdrl ws))}
  (Txfee c) -> tx {Sophie._txfee = (Sophie._txfee tx) <+> c}
  (Vldt (ValidityInterval (SJust n) _)) -> tx {Sophie._ttl = n}
  (Vldt (ValidityInterval SNothing _)) -> tx {Sophie._ttl = 0}
  (Update up) -> tx {Sophie._txUpdate = applySMaybe p (Sophie._txUpdate tx) up}
  (AdHash hs) -> tx {Sophie._mdHash = applySMaybe p (Sophie._mdHash tx) hs}
  _ -> tx
updateTxBody p (Evie _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Collateral is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins (applyMerge p (MA.outputs' tx) outs1) certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs (applyMerge p (MA.certs' tx) cs) wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs (Wdrl (Map.unionWith (<+>) (unWdrl (MA.wdrls' tx)) (unWdrl ws))) txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls ((MA.txfee' tx) <+> c) vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt (applySMaybe p ups up) adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups (applySMaybe p adHash hs) mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody p (Jen _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Collateral is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins (applyMerge p (MA.outputs' tx) outs1) certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs (applyMerge p (MA.certs' tx) cs) wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs (Wdrl (Map.unionWith (<+>) (unWdrl (MA.wdrls' tx)) (unWdrl ws))) txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls ((MA.txfee' tx) <+> c) vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt (applySMaybe p ups up) adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups (applySMaybe p adHash hs) mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody p (Aurum _) tx dt = case dt of
  (Inputs is) -> tx {Aurum.inputs = applyMerge p (Aurum.inputs tx) is}
  (Collateral is) -> tx {Aurum.collateral = applyMerge p (Aurum.collateral tx) is}
  (Outputs outs1) -> tx {Aurum.outputs = applyMerge p (Aurum.outputs tx) outs1}
  (Certs cs) -> tx {Aurum.txcerts = applyMerge p (Aurum.txcerts tx) cs}
  (Wdrls ws) -> tx {Aurum.txwdrls = Wdrl (Map.unionWith (<+>) (unWdrl (Aurum.txwdrls tx)) (unWdrl ws))}
  (Txfee c) -> tx {Aurum.txfee = (Aurum.txfee tx) <+> c}
  (Vldt vi) -> tx {Aurum.txvldt = vi}
  (Update up) -> tx {Aurum.txUpdates = applySMaybe p (Aurum.txUpdates tx) up}
  (ReqSignerHashes hs) -> tx {Aurum.reqSignerHashes = applyMerge p (Aurum.reqSignerHashes tx) hs}
  (Mint v) -> tx {Aurum.mint = v}
  (WppHash h) -> tx {Aurum.scriptIntegrityHash = applySMaybe p (Aurum.scriptIntegrityHash tx) h}
  (AdHash hs) -> tx {Aurum.adHash = applySMaybe p (Aurum.adHash tx) hs}
  (Txnetworkid i) -> tx {Aurum.txnetworkid = i}

newTxBody :: Era era => Policy -> Proof era -> [TxBodyField era] -> Core.TxBody era
newTxBody p era = List.foldl' (updateTxBody p era) (initialTxBody era)

--------------------------------------------------------------------
-- Updaters for Witnesses

data WitnessesField era
  = AddrWits [WitVKey 'Witness (Crypto era)]
  | BootWits [BootstrapWitness (Crypto era)]
  | ScriptWits [Core.Script era]
  | DataWits [Data era]
  | RdmrWits (Redeemers era)

hashpair ::
  forall era.
  (ValidateScript era) =>
  Proof era ->
  Core.Script era ->
  (ScriptHash (Crypto era), Core.Script (era))
hashpair _ x = (hashScript @era x, x)

updateWitnesses :: forall era. Policy -> Proof era -> Core.Witnesses era -> WitnessesField era -> Core.Witnesses era
updateWitnesses p era@(Sophie _) w dw = case dw of
  (AddrWits ks) -> w {Sophie.addrWits = applyMerge p (Sophie.addrWits w) ks}
  (BootWits boots) -> w {Sophie.bootWits = applyMerge p (Sophie.bootWits w) boots}
  (ScriptWits ss) -> w {Sophie.scriptWits = applyMap "ScriptWits" p (Sophie.scriptWits w) (map (hashpair era) ss)}
  _ -> w
updateWitnesses p era@(Evie _) w dw = case dw of
  (AddrWits ks) -> w {Sophie.addrWits = applyMerge p (Sophie.addrWits w) ks}
  (BootWits boots) -> w {Sophie.bootWits = applyMerge p (Sophie.bootWits w) boots}
  (ScriptWits ss) -> w {Sophie.scriptWits = applyMap "ScriptWits" p (Sophie.scriptWits w) (map (hashpair era) ss)}
  _ -> w
updateWitnesses p era@(Jen _) w dw = case dw of
  (AddrWits ks) -> w {Sophie.addrWits = applyMerge p (Sophie.addrWits w) ks}
  (BootWits boots) -> w {Sophie.bootWits = applyMerge p (Sophie.bootWits w) boots}
  (ScriptWits ss) -> w {Sophie.scriptWits = applyMap "ScriptWits" p (Sophie.scriptWits w) (map (hashpair era) ss)}
  _ -> w
updateWitnesses p wit@(Aurum _) w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = applyMerge p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = applyMerge p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = applyMap "ScriptWits" p (txscripts w) (map (hashpair wit) ss)}
  (DataWits ds) ->
    w
      { txdats = TxDats $ applyMap "DataWits" p (unTxDats $ txdats w) (map (\x -> (hashData @era x, x)) ds)
      }
  (RdmrWits r) -> w {txrdmrs = r} -- We do not use a merging sematics on Redeemers because the Hashes would get messed up.

newWitnesses :: Era era => Policy -> Proof era -> [WitnessesField era] -> Core.Witnesses era
newWitnesses p era = List.foldl' (updateWitnesses p era) (initialWitnesses era)

-- =====================================================

data PParamsField era
  = MinfeeA (Natural)
  | -- | The constant factor for the minimum fee calculation
    MinfeeB (Natural)
  | -- | Maximal block body size
    MaxBBSize (Natural)
  | -- | Maximal transaction size
    MaxTxSize (Natural)
  | -- | Maximal block header size
    MaxBHSize (Natural)
  | -- | The amount of a key registration deposit
    KeyDeposit (Coin)
  | -- | The amount of a pool registration deposit
    PoolDeposit (Coin)
  | -- | epoch bound on pool retirement
    EMax (EpochNo)
  | -- | Desired number of pools
    NOpt (Natural)
  | -- | Pool influence
    A0 (NonNegativeInterval)
  | -- | Monetary expansion
    Rho (UnitInterval)
  | -- | Treasury expansion
    Tau (UnitInterval)
  | -- | Decentralization parameter
    D (UnitInterval)
  | -- | Extra entropy
    ExtraEntropy (Nonce)
  | -- | Protocol version
    ProtocolVersion (ProtVer)
  | -- | Minimum Stake Pool Cost
    MinPoolCost (Coin)
  | -- | Cost in bcc per byte of UTxO storage (instead of _minUTxOValue)
    BccPerUTxOWord (Coin)
  | -- | Cost models for non-native script languages
    Costmdls ((Map Language CostModel))
  | -- | Prices of execution units (for non-native script languages)
    Prices (Prices)
  | -- | Max total script execution resources units allowed per tx
    MaxTxExUnits (ExUnits)
  | -- | Max total script execution resources units allowed per block
    MaxBlockExUnits (ExUnits)
  | -- | Max size of a Value in an output
    MaxValSize (Natural)
  | -- | The scaling percentage of the collateral relative to the fee
    CollateralPercentage (Natural)

-- | An updater specialized to the Sophie PParams (also used in Evie and Jen)
updateSophiePP :: PP.PParams era -> PParamsField era -> PP.PParams era
updateSophiePP pp dpp = case dpp of
  (MinfeeA nat) -> pp {PP._minfeeA = nat}
  (MinfeeB nat) -> pp {PP._minfeeB = nat}
  (MaxBBSize nat) -> pp {PP._maxBBSize = nat}
  (MaxTxSize nat) -> pp {PP._maxTxSize = nat}
  (MaxBHSize nat) -> pp {PP._maxBHSize = nat}
  (KeyDeposit coin) -> pp {PP._keyDeposit = coin}
  (PoolDeposit coin) -> pp {PP._poolDeposit = coin}
  (EMax e) -> pp {PP._eMax = e}
  (NOpt nat) -> pp {PP._nOpt = nat}
  (A0 rat) -> pp {PP._a0 = rat}
  (Rho u) -> pp {PP._rho = u}
  (Tau u) -> pp {PP._tau = u}
  (D u) -> pp {PP._d = u}
  (ExtraEntropy nonce) -> pp {PP._extraEntropy = nonce}
  (ProtocolVersion pv) -> pp {PP._protocolVersion = pv}
  (MinPoolCost coin) -> pp {PP._minPoolCost = coin}
  _ -> pp

-- | updatePParams uses the Override policy exclusively
updatePParams :: Proof era -> Core.PParams era -> PParamsField era -> Core.PParams era
updatePParams (Sophie _) pp dpp = updateSophiePP pp dpp
updatePParams (Evie _) pp dpp = updateSophiePP pp dpp
updatePParams (Jen _) pp dpp = updateSophiePP pp dpp
updatePParams (Aurum _) pp dpp = case dpp of
  (MinfeeA nat) -> pp {Aurum._minfeeA = nat}
  (MinfeeB nat) -> pp {Aurum._minfeeB = nat}
  (MaxBBSize nat) -> pp {Aurum._maxBBSize = nat}
  (MaxTxSize nat) -> pp {Aurum._maxTxSize = nat}
  (MaxBHSize nat) -> pp {Aurum._maxBHSize = nat}
  (KeyDeposit coin) -> pp {Aurum._keyDeposit = coin}
  (PoolDeposit coin) -> pp {Aurum._poolDeposit = coin}
  (EMax e) -> pp {Aurum._eMax = e}
  (NOpt nat) -> pp {Aurum._nOpt = nat}
  (A0 rat) -> pp {Aurum._a0 = rat}
  (Rho u) -> pp {Aurum._rho = u}
  (Tau u) -> pp {Aurum._tau = u}
  (D u) -> pp {Aurum._d = u}
  (ExtraEntropy nonce) -> pp {Aurum._extraEntropy = nonce}
  (ProtocolVersion pv) -> pp {Aurum._protocolVersion = pv}
  (MinPoolCost coin) -> pp {Aurum._minPoolCost = coin}
  Costmdls cost -> pp {Aurum._costmdls = cost}
  MaxValSize n -> pp {Aurum._maxValSize = n}
  MaxTxExUnits n -> pp {Aurum._maxTxExUnits = n}
  MaxBlockExUnits n -> pp {Aurum._maxBlockExUnits = n}
  CollateralPercentage perc -> pp {Aurum._collateralPercentage = perc}
  _ -> pp

newPParams :: Proof era -> [PParamsField era] -> Core.PParams era
newPParams era = List.foldl' (updatePParams era) (initialPParams era)

--------------------------------------------------------------------
-- Updaters for TxOut

notAddress :: TxOutField era -> Bool
notAddress (Address _) = False
notAddress _ = True

applyValue :: Policy -> Proof era -> Core.Value era -> Core.Value era -> Core.Value era
applyValue Override _ _old new = new
applyValue NoDups _ _old new = new
applyValue Merge (Sophie _) old new = old <+> new
applyValue Merge (Evie _) old new = old <+> new
applyValue Merge (Jen _) old new = old <+> new
applyValue Merge (Aurum _) old new = old <+> new

data TxOutField era
  = Address (Addr (Crypto era))
  | Amount (Core.Value era)
  | DHash [DataHash (Crypto era)] -- 0 or 1 element, represents Maybe type

updateTxOut :: Policy -> Proof era -> Core.TxOut era -> TxOutField era -> Core.TxOut era
updateTxOut p (Sophie c) (out@(Sophie.TxOut a v)) txoutd = case txoutd of
  Address addr -> Sophie.TxOut addr v
  Amount val -> Sophie.TxOut a (applyValue p (Sophie c) v val)
  _ -> out
updateTxOut p (Evie c) (out@(Sophie.TxOut a v)) txoutd = case txoutd of
  Address addr -> Sophie.TxOut addr v
  Amount val -> Sophie.TxOut a (applyValue p (Evie c) v val)
  _ -> out
updateTxOut p (Jen c) (out@(Sophie.TxOut a v)) txoutd = case txoutd of
  Address addr -> Sophie.TxOut addr v
  Amount val -> Sophie.TxOut a (applyValue p (Jen c) v val)
  _ -> out
updateTxOut p (Aurum c) (Aurum.TxOut a v h) txoutd = case txoutd of
  Address addr -> Aurum.TxOut addr v h
  Amount val -> Aurum.TxOut a (applyValue p (Aurum c) v val) h
  DHash mdh -> Aurum.TxOut a v (applySMaybe Merge h mdh)

newTxOut :: Era era => Policy -> Proof era -> [TxOutField era] -> Core.TxOut era
newTxOut _ _ dts | all notAddress dts = error ("A call to newTxOut must have an (Address x) field.")
newTxOut p era dts = List.foldl' (updateTxOut p era) (initialTxOut era) dts

-- | A Meaningless Addr.
initialAddr :: Era era => Proof era -> Addr (Crypto era)
initialAddr _wit = Addr Testnet pCred sCred
  where
    (KeyPair svk _ssk) = theKeyPair 0
    pCred = KeyHashObj . hashKey . vKey $ theKeyPair 1
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

initialTxOut :: Era era => Proof era -> Core.TxOut era
initialTxOut wit@(Sophie _) = Sophie.TxOut (initialAddr wit) (Coin 0)
initialTxOut wit@(Evie _) = Sophie.TxOut (initialAddr wit) (Coin 0)
initialTxOut wit@(Jen _) = Sophie.TxOut (initialAddr wit) (inject (Coin 0))
initialTxOut wit@(Aurum _) = Aurum.TxOut (initialAddr wit) (inject (Coin 0)) SNothing

-- ====================================

-- | This only make sense in the Aurum era, all other Eras return Nothing
newScriptIntegrityHash ::
  Proof era ->
  Core.PParams era ->
  [Language] ->
  Redeemers era ->
  TxDats era ->
  [Aurum.ScriptIntegrityHash (Crypto era)] -- always of length 0 or 1
newScriptIntegrityHash (Aurum _) pp ls rds dats =
  case (hashScriptIntegrity pp (Set.fromList ls) rds dats) of
    SJust x -> [x]
    SNothing -> []
newScriptIntegrityHash _wit _pp _ls _rds _dats = []

vkey :: Era era => Int -> Proof era -> VKey 'Witness (Crypto era)
vkey n _w = theVKey n
