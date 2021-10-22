{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Ledger.DescribeEras
  ( Witness (..),
    Checks (..),
    DescribesSophie,
    DescribesEvie,
    DescribesJen,
    DescribesAurum,
    StandardCrypto,
  )
where

import qualified Bcc.Ledger.Evie as Evie
import qualified Bcc.Ledger.Aurum as Aurum
import Bcc.Ledger.Core as Core
import Bcc.Ledger.Crypto (StandardCrypto)
import Bcc.Ledger.Era (WellFormed)
import qualified Bcc.Ledger.Jen as Jen
import qualified Bcc.Ledger.Sophie as Sophie

type DescribesSophie era =
  ( WellFormed era,
    Core.Value era ~ Sophie.Value era,
    Core.TxBody era ~ Sophie.TxBody era,
    Core.TxOut era ~ Sophie.TxOut era,
    Core.Script era ~ Sophie.Script era,
    Core.AuxiliaryData era ~ Sophie.AuxiliaryData era,
    Core.PParams era ~ Sophie.PParams era,
    Core.PParamsDelta era ~ Sophie.PParamsDelta era
  )

type DescribesEvie era =
  ( WellFormed era,
    Core.Value era ~ Sophie.Value era,
    Core.TxBody era ~ Evie.TxBody era,
    Core.TxOut era ~ Sophie.TxOut era,
    Core.Script era ~ Evie.Script era,
    Core.AuxiliaryData era ~ Evie.AuxiliaryData era,
    Core.PParams era ~ Sophie.PParams era,
    Core.PParamsDelta era ~ Sophie.PParamsDelta era
  )

type DescribesJen era =
  ( WellFormed era,
    Core.Value era ~ Jen.Value era,
    Core.TxBody era ~ Evie.TxBody era,
    Core.TxOut era ~ Sophie.TxOut era,
    Core.Script era ~ Evie.Script era,
    Core.AuxiliaryData era ~ Evie.AuxiliaryData era,
    Core.PParams era ~ Sophie.PParams era,
    Core.PParamsDelta era ~ Sophie.PParamsDelta era
  )

type DescribesAurum era =
  ( WellFormed era,
    Core.Value era ~ Jen.Value era,
    Core.TxBody era ~ Aurum.TxBody era,
    Core.TxOut era ~ Aurum.TxOut era,
    Core.Script era ~ Aurum.Script era,
    Core.AuxiliaryData era ~ Aurum.AuxiliaryData era,
    Core.PParams era ~ Aurum.PParams era,
    Core.PParamsDelta era ~ Aurum.PParamsDelta era
  )

-- | If an instance for this class compiles, then era meets whatever superclass its given.
class Checks era where
  checks :: Witness era -> Bool

instance DescribesSophie (Sophie.Self c) => Checks (Sophie.Self c) where
  checks Sophie = True

instance DescribesEvie (Evie.Self c) => Checks (Evie.Self c) where
  checks Evie = True

instance DescribesJen (Jen.Self c) => Checks (Jen.Self c) where
  checks Jen = True

instance DescribesAurum (Aurum.Self c) => Checks (Aurum.Self c) where
  checks Aurum = True

-- ==========================================================

-- | Witness of a valid (predefined) era
data Witness era where
  Sophie :: Witness (Sophie.SophieEra StandardCrypto)
  Jen :: Witness (Jen.JenEra StandardCrypto)
  Evie :: Witness (Evie.EvieEra StandardCrypto)
  Aurum :: Witness (Aurum.AurumEra StandardCrypto)

instance Show (Witness e) where
  show = \case
    Sophie -> "Sophie"
    Evie -> "Evie"
    Jen -> "Jen"
    Aurum -> "Aurum"
