{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the sophie era, along with instances ot the @Core@ types
-- defined in @module Bcc.Ledger.Core@, and instances of the @API@ classes
-- exposed in @module Sophie.Spec.Ledger.API@.
module Bcc.Ledger.Sophie
  ( SophieEra,
    Self,
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    Core.PParamsDelta,
    Tx,
    Witnesses,
    nativeMultiSigTag,
  )
where

import Bcc.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import Bcc.Ledger.Coin (Coin)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (SupportsSegWit (..), ValidateScript (..))
import qualified Bcc.Ledger.Era as E (Era (Crypto), TranslationContext)
import Bcc.Ledger.Hashes (EraIndependentAuxiliaryData)
import Bcc.Ledger.SafeHash (makeHashWithExplicitProxys)
import Bcc.Ledger.Sophie.Constraints
  ( UsesPParams (..),
    UsesTxBody,
    UsesTxOut (..),
    UsesValue,
  )
import qualified Data.ByteString as BS
import Data.Proxy
import Sophie.Spec.Ledger.BlockChain (bbHash)
import qualified Sophie.Spec.Ledger.BlockChain as Sophie
  ( TxSeq (..),
    txSeqTxns,
  )
import Sophie.Spec.Ledger.Metadata (Metadata (Metadata), validMetadatum)
import Sophie.Spec.Ledger.PParams (PParams, PParamsUpdate, updatePParams)
import Sophie.Spec.Ledger.Scripts (MultiSig)
import Sophie.Spec.Ledger.Tx
  ( WitnessSet,
    validateNativeMultiSigScript,
  )
import qualified Sophie.Spec.Ledger.Tx as STx (Tx, TxBody, TxOut (..))

data SophieEra c

instance CryptoClass.Crypto c => E.Era (SophieEra c) where
  type Crypto (SophieEra c) = c

instance CryptoClass.Crypto c => UsesValue (SophieEra c)

instance CryptoClass.Crypto c => UsesTxOut (SophieEra c) where
  makeTxOut _ a v = STx.TxOut a v

instance CryptoClass.Crypto c => UsesPParams (SophieEra c) where
  mergePPUpdates _ = updatePParams

type instance E.TranslationContext (SophieEra c) = ()

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Tx (SophieEra c) = STx.Tx (SophieEra c)

type instance Core.Value (SophieEra _c) = Coin

type instance Core.TxBody (SophieEra c) = STx.TxBody (SophieEra c)

type instance Core.TxOut (SophieEra c) = STx.TxOut (SophieEra c)

type instance Core.Script (SophieEra c) = MultiSig c

type instance Core.AuxiliaryData (SophieEra c) = Metadata (SophieEra c)

type instance Core.PParams (SophieEra c) = PParams (SophieEra c)

type instance Core.Witnesses (SophieEra c) = WitnessSet (SophieEra c)

type instance Core.PParamsDelta (SophieEra c) = PParamsUpdate (SophieEra c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- | Magic number "memorialized" in the ValidateScript class under the method:
--   scriptPrefixTag:: Core.Script era -> Bs.ByteString, for the Sophie Era.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

instance
  (CryptoClass.Crypto c, UsesTxBody (SophieEra c)) =>
  ValidateScript (SophieEra c)
  where
  scriptPrefixTag _script = nativeMultiSigTag

  -- In the SophieEra there is only one kind of Script and its tag is "\x00"
  validateScript = validateNativeMultiSigScript

instance CryptoClass.Crypto c => SupportsSegWit (SophieEra c) where
  type TxSeq (SophieEra c) = Sophie.TxSeq (SophieEra c)
  fromTxSeq = Sophie.txSeqTxns
  toTxSeq = Sophie.TxSeq
  hashTxSeq = bbHash
  numSegComponents = 3

instance CryptoClass.Crypto c => ValidateAuxiliaryData (SophieEra c) c where
  validateAuxiliaryData (Metadata m) = all validMetadatum m
  hashAuxiliaryData metadata = AuxiliaryDataHash (makeHashWithExplicitProxys (Proxy @c) index metadata)
    where
      index = Proxy @EraIndependentAuxiliaryData

-- Self describing synonyms

type Value era = Coin

type Script era = MultiSig (E.Crypto era)

type AuxiliaryData era = Metadata era

type Self c = SophieEra c

type Tx era = STx.Tx era

type TxOut era = STx.TxOut era

type TxBody era = STx.TxBody era

type Witnesses era = WitnessSet (E.Crypto era)
