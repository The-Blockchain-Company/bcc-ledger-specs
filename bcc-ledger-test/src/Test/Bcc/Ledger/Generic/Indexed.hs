{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Bcc.Ledger.Generic.Indexed where

import Bcc.Crypto.DSIGN.Class ()
import qualified Bcc.Crypto.Hash as CH
import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Scripts (Script (..), alwaysFails, alwaysSucceeds)
import Bcc.Ledger.Coin (Coin (..))
import Bcc.Ledger.Core (Value)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Bcc.Ledger.Crypto as CC (Crypto, HASH)
import Bcc.Ledger.Era (Era (..), ValidateScript (..))
import Bcc.Ledger.Hashes (EraIndependentTxBody, ScriptHash (..))
import Bcc.Ledger.Keys
import qualified Bcc.Ledger.Jen.Value as Jen (AssetName (..), PolicyID (..), Value (..))
import Bcc.Ledger.Pretty (PrettyA (..), ppPair, ppString)
import Bcc.Ledger.SafeHash (SafeHash)
import Bcc.Ledger.SophieMA.Timelocks (Timelock (..))
import Bcc.Slotting.Slot (SlotNo (..))
import Data.ByteString (ByteString, pack, unpack)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as Seq (fromList)
import Sophie.Spec.Ledger.Scripts (MultiSig)
import qualified Sophie.Spec.Ledger.Scripts as Multi
import Sophie.Spec.Ledger.TxBody (WitVKey (..))
import Sophie.Spec.Ledger.UTxO (makeWitnessVKey)
import Test.Bcc.Ledger.Generic.Proof
import Test.Sophie.Spec.Ledger.SentryUtils (RawSeed (..), mkKeyPair)

-- ===========================================================================
-- Classes for "picking" the unique element of a type associated with an Int

class PrettyA t => Fixed t where
  unique :: Int -> t
  size :: Proxy t -> Maybe Int
  size _ = Nothing

class Era e => IndexedE t e where
  pickE :: Int -> Proof e -> (t e)

pickCbyCrypto :: (Fixed (t c)) => Int -> Evidence c -> (t c)
pickCbyCrypto n Standard = unique n
pickCbyCrypto n Mock = unique n

pickCbyEra :: (Fixed (t (Crypto era))) => Int -> Proof era -> (t (Crypto era))
pickCbyEra n _ = unique n

-- =======================================================
-- Examples where the type is independent of Era

names :: ByteString
names = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

allnames :: [ByteString]
allnames = (map (\x -> pack [x]) (unpack names))

instance Fixed Jen.AssetName where
  unique n = (map Jen.AssetName allnames) !! n
  size _ = Just (length allnames)

instance Fixed Coin where
  size _ = Nothing
  unique n = Coin (fromIntegral n)

-- =======================================================
-- Examples where type depends on Crypto

data MultiAsset era where
  MultiAsset :: Era era => Jen.Value (Crypto era) -> MultiAsset era

unMulti :: MultiAsset era -> Jen.Value (Crypto era)
unMulti (MultiAsset x) = x

deriving instance Show (MultiAsset era)

instance (Reflect era, ValidateScript era, Fixed (Core.Script era)) => Fixed (MultiAsset era) where
  unique n =
    MultiAsset
      ( Jen.Value
          (fromIntegral n)
          ( Map.singleton
              (lift (pickPolicyID @era n))
              (Map.singleton (unique @Jen.AssetName n) (fromIntegral n))
          )
      )
  size _ = lift (scriptsize @era)

scriptsize :: forall era. Fixed (Core.Script era) => Proof era -> Maybe Int
scriptsize _ = size (Proxy @(Core.Script era))

instance CC.Crypto c => Fixed (Jen.Value c) where
  size _ = Nothing
  unique n = Jen.Value (fromIntegral n) Map.empty

-- =======================================================
-- Keys and KeyHashes

-- | A signing key
newtype SKey (kr :: KeyRole) c = SKey (SignKeyDSIGN c)

instance CC.Crypto c => Fixed (KeyPair kr c) where
  unique n = (KeyPair a b)
    where
      m1 = fromIntegral n
      (b, a) = mkKeyPair (RawSeed 0 0 0 0 m1)

theKeyPair :: CC.Crypto c => Int -> KeyPair kr c
theKeyPair = unique

theVKey :: CC.Crypto c => Int -> VKey kr c
theVKey n = vKey (theKeyPair n)

theSKey :: forall c kr. CC.Crypto c => Int -> SKey kr c
theSKey n = SKey (sKey (theKeyPair @c n))

theKeyHash :: CC.Crypto c => Int -> KeyHash kr c
theKeyHash n = hashKey (theVKey n)

theWitVKey :: (CC.Crypto c, Good c) => Int -> SafeHash c EraIndependentTxBody -> WitVKey 'Witness c
theWitVKey n hash = makeWitnessVKey hash (theKeyPair n)

theKeyHashObj :: CC.Crypto crypto => Int -> Credential kr crypto
theKeyHashObj n = KeyHashObj . hashKey . vKey $ theKeyPair n

aScriptHashObj :: forall era kr. ValidateScript era => Proof era -> Core.Script era -> Credential kr (Crypto era)
aScriptHashObj _wit s = ScriptHashObj . hashScript @era $ s

theStakeReference :: CC.Crypto crypto => Int -> StakeReference crypto
theStakeReference n = (StakeRefBase . KeyHashObj . hashKey) (theVKey n)

-- ====================================================
-- SlotNo

instance Fixed SlotNo where
  unique n = SlotNo (fromIntegral n)

-- ==========================================================================
-- An example where there is no easy algorithmic way to pick a (T era) from
-- an Int, so we compute a "basket" of (T era) and then just choose one from
-- the "basket".  We do this for all of the kinds of scripts.
-- ==========================================================================
-- MultiSig Scripts

multisigSimple :: forall c. CC.Crypto c => Evidence c -> [MultiSig c]
multisigSimple _c =
  [ Multi.RequireAnyOf mempty, -- always False
    Multi.RequireAllOf mempty -- always True
  ]

multisigFrom :: forall c. CC.Crypto c => Evidence c -> Int -> [MultiSig c]
multisigFrom _c n =
  [ Multi.RequireSignature (theKeyHash n)
  ]

multisigCompound :: forall c. CC.Crypto c => Evidence c -> Int -> [MultiSig c]
multisigCompound c n =
  [ Multi.RequireAnyOf (multisigFrom c n),
    Multi.RequireAllOf (multisigFrom c n),
    Multi.RequireMOf 1 (multisigFrom c n),
    Multi.RequireMOf 2 (multisigFrom c n)
  ]

somemultisigs :: CC.Crypto c => Evidence c -> [MultiSig c]
somemultisigs c =
  multisigSimple c
    ++ concat [multisigFrom c i | i <- [1 .. 5]]
    ++ concat [multisigCompound c i | i <- [1 .. 5]]

instance ReflectC c => Fixed (MultiSig c) where
  unique n = (somemultisigs evidence) !! n
  size _ = Just multisiglength

multisiglength :: Int
multisiglength = length (somemultisigs Mock) - 1

-- ====================================================
-- Timelock Scripts

timelockSimple :: forall c. CC.Crypto c => Evidence c -> [Timelock c]
timelockSimple _c =
  [ RequireAnyOf mempty, -- always False
    RequireAllOf mempty -- always True
  ]

timelockFrom :: forall c. CC.Crypto c => Evidence c -> Int -> [Timelock c]
timelockFrom _c n =
  [ RequireSignature (theKeyHash n),
    RequireTimeExpire (unique @SlotNo n),
    RequireTimeStart (unique @SlotNo n)
  ]

timelockCompound :: forall c. CC.Crypto c => Evidence c -> Int -> [Timelock c]
timelockCompound c n =
  [ RequireAnyOf (Seq.fromList (timelockFrom c n)),
    RequireAllOf (Seq.fromList (timelockFrom c n)),
    RequireMOf 1 (Seq.fromList (timelockFrom c n)),
    RequireMOf 2 (Seq.fromList (timelockFrom c n))
  ]

sometimelocks :: CC.Crypto c => Evidence c -> [Timelock c]
sometimelocks c =
  timelockSimple c
    ++ concat [timelockFrom c i | i <- [1 .. 5]]
    ++ concat [timelockCompound c i | i <- [1 .. 5]]

timelocklength :: Int
timelocklength = length (sometimelocks Mock) -1

instance (ReflectC c, CC.Crypto c) => Fixed (Timelock c) where
  unique n = (liftC sometimelocks) !! n
  size _ = Just timelocklength

-- ====================================================
-- Aurum Scripts

aurumSimple :: forall era. [Script era]
aurumSimple =
  [ alwaysFails 1, -- always False
    alwaysSucceeds 1 -- always True
  ]

someaurum :: Era era => Evidence (Crypto era) -> [Script era]
someaurum c =
  aurumSimple
    ++ ( fmap
           TimelockScript
           ( concat [timelockFrom c i | i <- [1 .. 5]]
               ++ concat [timelockCompound c i | i <- [1 .. 5]]
           )
       )

aurumlength :: Int
aurumlength = length (someaurum Mock :: [Script (AurumEra Mock)]) -1

instance Reflect (AurumEra c) => Fixed (Script (AurumEra c)) where
  unique n = (liftC someaurum) !! n
  size _ = Just aurumlength

-- ==============================================
-- Type families (and other Types uniquely determined from type families like Hashes)
-- Because we can't make instances over type families, we can't say things like
-- instance IndexedE (Core.Value era) where pickE x wit = ...
-- So instead we make a special pickXXX function where XXX is the name of a type family

pickValue :: forall era. Reflect era => Int -> Proof era -> (Value era)
pickValue n (Sophie _) = unique @Coin n
pickValue n (Evie _) = unique @Coin n
pickValue n (Jen _) = unMulti (unique @(MultiAsset era) n)
pickValue n (Aurum _) = unMulti (unique @(MultiAsset era) n)

pickScript :: Int -> Proof era -> Core.Script era
pickScript n (Sophie c) = somemultisigs c !! n
pickScript n (Evie c) = sometimelocks c !! n
pickScript n (Jen c) = sometimelocks c !! n
pickScript n (Aurum c) = someaurum c !! n

pickScriptHash :: forall era. Reflect era => Int -> Proof era -> ScriptHash (Crypto era)
pickScriptHash n wit = hashScript @era (pickScript n wit)

pickPolicyID :: Reflect era => Int -> Proof era -> Jen.PolicyID (Crypto era)
pickPolicyID n wit = Jen.PolicyID (pickScriptHash n wit)

-- ===========================================================================
-- An example where the 'pick' is the same type across all Crypto Evidence

data PublicSecret kr kr' crypto = PublicSecret (KeyPair kr crypto) (KeyPair kr' crypto)

instance CC.Crypto c => Fixed (PublicSecret kr kr' c) where
  unique n = PublicSecret (KeyPair a b) (KeyPair c d)
    where
      m1 = fromIntegral (2 * n)
      m2 = fromIntegral (2 * n + 1)
      (b, a) = mkKeyPair (RawSeed m1 m1 m1 m1 m1)
      (d, c) = mkKeyPair (RawSeed m2 m2 m2 m2 m2)

-- ===============================================================
-- PrettyA instances

instance (PrettyA x, PrettyA y) => PrettyA (x, y) where
  prettyA = ppPair prettyA prettyA

instance (CC.Crypto c) => PrettyA (PublicSecret kr kr' c) where
  prettyA (PublicSecret x y) = ppPair prettyA prettyA (x, y)

instance PrettyA (SKey kr c) where
  prettyA (SKey _x) = ppString ("SKey")

instance PrettyA (MultiAsset era) where
  prettyA (MultiAsset v) = prettyA v

type Good c = DSignable c (CH.Hash (CC.HASH c) EraIndependentTxBody)
