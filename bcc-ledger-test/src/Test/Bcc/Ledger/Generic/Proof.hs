{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Generic.Proof where

import Bcc.Ledger.Evie (EvieEra)
import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Crypto (StandardCrypto)
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Era (Era (..), ValidateScript (..))
import Bcc.Ledger.Jen (JenEra)
import Bcc.Ledger.Sophie (SophieEra)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)

-- =================================================
-- GADTs for witnessing Crypto and Era

type Mock = C_Crypto

type Standard = StandardCrypto

type SophieMock = SophieEra Mock

type SophieReal = SophieEra Standard

type EvieMock = EvieEra Mock

type EvieReal = EvieEra Standard

type JenMock = JenEra Mock

type JenReal = JenEra Standard

type AurumMock = AurumEra Mock

type AurumReal = AurumEra Standard

data Evidence c where
  Standard :: Evidence Standard
  Mock :: Evidence Mock

-- | Proof of a valid (predefined) era
data Proof era where
  Sophie :: forall c. CC.Crypto c => Evidence c -> Proof (SophieEra c)
  Jen :: forall c. CC.Crypto c => Evidence c -> Proof (JenEra c)
  Evie :: forall c. CC.Crypto c => Evidence c -> Proof (EvieEra c)
  Aurum :: forall c. CC.Crypto c => Evidence c -> Proof (AurumEra c)

instance Show (Proof e) where
  show (Sophie c) = "Sophie " ++ show c
  show (Evie c) = "Evie " ++ show c
  show (Jen c) = "Jen " ++ show c
  show (Aurum c) = "Aurum " ++ show c

instance Show (Evidence c) where
  show Mock = "Mock"
  show Standard = "Standard"

-- ==================================
-- Reflection over Crypto and Era

class CC.Crypto c => ReflectC c where
  evidence :: Evidence c
  liftC :: forall a. (Evidence c -> a) -> a
  liftC f = f (evidence @c)

instance ReflectC StandardCrypto where
  evidence = Standard

instance ReflectC C_Crypto where
  evidence = Mock

class
  ( Era era,
    ValidateScript era,
    ReflectC (Crypto era)
  ) =>
  Reflect era
  where
  reify :: Proof era
  lift :: forall a. (Proof era -> a) -> a
  lift f = f (reify @era)

instance ReflectC c => Reflect (AurumEra c) where
  reify = Aurum evidence

instance ReflectC c => Reflect (JenEra c) where
  reify = Jen evidence

instance ReflectC c => Reflect (EvieEra c) where
  reify = Evie evidence

instance ReflectC c => Reflect (SophieEra c) where
  reify = Sophie evidence
