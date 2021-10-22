{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Ledger.Sophie.Constraints where

import Bcc.Binary (FromCBOR (..), ToCBOR (..))
import Bcc.Ledger.Address (Addr)
import Bcc.Ledger.AuxiliaryData (ValidateAuxiliaryData)
import Bcc.Ledger.Compactible (Compactible (..))
import Bcc.Ledger.Core
  ( AnnotatedData,
    AuxiliaryData,
    ChainData,
    PParams,
    PParamsDelta,
    Script,
    SerialisableData,
    TxBody,
    TxOut,
    Value,
  )
import Bcc.Ledger.Era (Crypto, Era)
import Bcc.Ledger.Hashes (EraIndependentTxBody)
import Bcc.Ledger.SafeHash (HashAnnotated)
import Bcc.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import GHC.Records (HasField)
import Sophie.Spec.Ledger.CompactAddr (CompactAddr)

--------------------------------------------------------------------------------
-- Sophie Era
--------------------------------------------------------------------------------

type UsesTxBody era =
  ( Era era,
    ChainData (TxBody era),
    AnnotatedData (TxBody era),
    HashAnnotated (TxBody era) EraIndependentTxBody (Crypto era)
  )

class
  ( Era era,
    Val (Value era),
    Compactible (Value era),
    ChainData (Value era),
    SerialisableData (Value era),
    DecodeNonNegative (Value era),
    EncodeMint (Value era),
    DecodeMint (Value era)
  ) =>
  UsesValue era

class
  ( Era era,
    ChainData (TxOut era),
    ToCBOR (TxOut era),
    FromCBOR (TxOut era),
    HasField "address" (TxOut era) (Addr (Crypto era)),
    HasField "compactAddress" (TxOut era) (CompactAddr (Crypto era)),
    HasField "value" (TxOut era) (Value era)
  ) =>
  UsesTxOut era
  where
  makeTxOut :: Proxy era -> Addr (Crypto era) -> Value era -> TxOut era

type UsesScript era =
  ( Era era,
    Eq (Script era),
    Show (Script era),
    AnnotatedData (Script era)
  )

type UsesAuxiliary era =
  ( Era era,
    Eq (AuxiliaryData era),
    Show (AuxiliaryData era),
    ValidateAuxiliaryData era (Crypto era),
    AnnotatedData (AuxiliaryData era)
  )

class
  ( Era era,
    Eq (PParams era),
    Show (PParams era),
    SerialisableData (PParams era),
    ChainData (PParamsDelta era),
    Ord (PParamsDelta era),
    SerialisableData (PParamsDelta era)
  ) =>
  UsesPParams era
  where
  mergePPUpdates ::
    proxy era ->
    PParams era ->
    PParamsDelta era ->
    PParams era

-- | Apply 'c' to all the types transitively involved with Value when
-- (Core.Value era) is an instance of Compactible
type TransValue (c :: Type -> Constraint) era =
  ( Era era,
    Compactible (Value era),
    c (Value era)
  )

-- | General constraints that will hold true for ledgers which are based on
-- Sophie, and share similar serialisation formats"
type SophieBased era =
  ( -- Value constraints
    UsesValue era,
    -- TxBody constraints
    UsesTxBody era,
    -- Script constraints
    UsesScript era,
    -- AuxiliaryData constraints
    UsesAuxiliary era
  )

{-# LANGUAGE Deprecated SophieBased "Use appropriate 'Uses' constraits (e.g. `UsesValue`) instead." #-}
