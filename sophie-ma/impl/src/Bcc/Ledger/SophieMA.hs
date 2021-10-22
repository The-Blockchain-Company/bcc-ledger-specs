{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Ledger.SophieMA
  ( SophieMAEra,
    JenOrEvie (..),
    TxOut,
    TxBody,
    AuxiliaryData,
    Sophie.PParams,
    Tx,
  )
where

import Bcc.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import Bcc.Ledger.Coin (Coin)
import Bcc.Ledger.Compactible (Compactible)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (Crypto, Era, SupportsSegWit (..), ValidateScript (..))
import Bcc.Ledger.Jen.Value (Value, policies, policyID)
import Bcc.Ledger.SafeHash (hashAnnotated)
import Bcc.Ledger.Sophie (nativeMultiSigTag)
import Bcc.Ledger.Sophie.Constraints
  ( UsesPParams (..),
    UsesTxBody,
    UsesTxOut (..),
    UsesValue,
  )
import Bcc.Ledger.SophieMA.AuxiliaryData
  ( AuxiliaryData,
    pattern AuxiliaryData,
  )
import Bcc.Ledger.SophieMA.Timelocks
  ( Timelock (..),
    validateTimelock,
  )
import Bcc.Ledger.SophieMA.TxBody (TxBody)
import Bcc.Ledger.Val (Val)
import Control.DeepSeq (deepseq)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import qualified Sophie.Spec.Ledger.BlockChain as Sophie
  ( TxSeq (..),
    bbHash,
    txSeqTxns,
  )
import Sophie.Spec.Ledger.Metadata (validMetadatum)
import qualified Sophie.Spec.Ledger.PParams as Sophie
import Sophie.Spec.Ledger.Scripts (ScriptHash)
import Sophie.Spec.Ledger.Tx (Tx, TxOut (..), WitnessSet)

-- ========================================

-- | The Sophie Jen/Evie eras
--   The uninhabited type that indexes both the Jen and Evie Eras.
data SophieMAEra (ma :: JenOrEvie) c

-- Both eras are implemented within the same codebase, matching the formal
-- specification. They differ only in the @value@ type. Due to some annoying
-- issues with 'Coin' and 'Value' being of different kinds, we don't parametrise
-- over the value but instead over a closed kind 'JenOrEvie'. But this
-- should be transparent to the user.
data JenOrEvie = Jen | Evie

-- | The MAClass provides a method and a type, which implement the differences
--   between the Jen and Evie instances
class
  ( Compactible (MAValue x c),
    Show (MAValue x c),
    Val (MAValue x c),
    Typeable x,
    CryptoClass.Crypto c
  ) =>
  MAClass (x :: JenOrEvie) c
  where
  type MAValue (x :: JenOrEvie) c :: Type
  getScriptHash :: Proxy x -> MAValue x c -> Set.Set (ScriptHash c)

instance CryptoClass.Crypto c => MAClass 'Jen c where
  type MAValue 'Jen c = Value c
  getScriptHash Proxy x = Set.map policyID (policies x)

instance CryptoClass.Crypto c => MAClass 'Evie c where
  type MAValue 'Evie c = Coin
  getScriptHash _ _ = Set.empty

-- | The actual Jen and Evie instances, rolled into one, the MAClass superclass
--   provides the era-specific code for where they differ.
instance
  forall c (ma :: JenOrEvie).
  (MAClass ma c) =>
  Era (SophieMAEra ma c)
  where
  type Crypto (SophieMAEra ma c) = c

instance CryptoClass.Crypto c => UsesValue (SophieMAEra 'Jen c)

instance CryptoClass.Crypto c => UsesValue (SophieMAEra 'Evie c)

instance CryptoClass.Crypto c => UsesTxOut (SophieMAEra 'Jen c) where
  makeTxOut _ a v = TxOut a v

instance CryptoClass.Crypto c => UsesTxOut (SophieMAEra 'Evie c) where
  makeTxOut _ a v = TxOut a v

instance CryptoClass.Crypto c => UsesPParams (SophieMAEra 'Jen c) where
  mergePPUpdates _ = Sophie.updatePParams

instance CryptoClass.Crypto c => UsesPParams (SophieMAEra 'Evie c) where
  mergePPUpdates _ = Sophie.updatePParams

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Value (SophieMAEra m c) = MAValue m c

type instance
  Core.Tx (SophieMAEra (ma :: JenOrEvie) c) =
    Tx (SophieMAEra ma c)

type instance
  Core.TxOut (SophieMAEra (ma :: JenOrEvie) c) =
    TxOut (SophieMAEra ma c)

type instance
  Core.TxBody (SophieMAEra (ma :: JenOrEvie) c) =
    TxBody (SophieMAEra ma c)

type instance
  Core.Script (SophieMAEra (_ma :: JenOrEvie) c) =
    Timelock c

type instance
  Core.AuxiliaryData (SophieMAEra (ma :: JenOrEvie) c) =
    AuxiliaryData (SophieMAEra (ma :: JenOrEvie) c)

type instance
  Core.PParams (SophieMAEra (ma :: JenOrEvie) c) =
    Sophie.PParams (SophieMAEra (ma :: JenOrEvie) c)

type instance
  Core.Witnesses (SophieMAEra (ma :: JenOrEvie) c) =
    WitnessSet (SophieMAEra (ma :: JenOrEvie) c)

type instance
  Core.PParamsDelta (SophieMAEra (ma :: JenOrEvie) c) =
    Sophie.PParamsUpdate (SophieMAEra (ma :: JenOrEvie) c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- Since Timelock scripts are a strictly backwards compatible extension of
-- Multisig scripts, we can use the same 'scriptPrefixTag' tag here as
-- we did for the ValidateScript instance in Multisig which is imported
-- from:  Bcc.Ledger.Sophie(nativeMultiSigTag)

instance
  ( CryptoClass.Crypto c,
    UsesTxBody (SophieMAEra ma c),
    Core.AnnotatedData (Core.AuxiliaryData (SophieMAEra ma c))
  ) =>
  ValidateScript (SophieMAEra ma c)
  where
  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"
  validateScript script tx = validateTimelock @(SophieMAEra ma c) script tx

-- Uses the default instance of hashScript

instance
  ( CryptoClass.Crypto c,
    MAClass ma c
  ) =>
  SupportsSegWit (SophieMAEra ma c)
  where
  type TxSeq (SophieMAEra ma c) = Sophie.TxSeq (SophieMAEra ma c)
  fromTxSeq = Sophie.txSeqTxns
  toTxSeq = Sophie.TxSeq
  hashTxSeq = Sophie.bbHash
  numSegComponents = 3

instance
  ( CryptoClass.Crypto c,
    Core.AnnotatedData (Core.Script (SophieMAEra ma c))
  ) =>
  ValidateAuxiliaryData (SophieMAEra (ma :: JenOrEvie) c) c
  where
  validateAuxiliaryData (AuxiliaryData md as) = deepseq as $ all validMetadatum md
  hashAuxiliaryData aux = AuxiliaryDataHash (hashAnnotated aux)

instance
  forall ma c.
  MAClass ma c =>
  HasField "minted" (TxBody (SophieMAEra (ma :: JenOrEvie) c)) (Set.Set (ScriptHash c))
  where
  getField x = getScriptHash (Proxy @ma) (getField @"mint" x)
