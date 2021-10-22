{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- This is needed to make Zerepoch.Data instances
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bcc.Ledger.Aurum.Data
  ( Data (Data, ..),
    DataHash,
    hashData,
    getZerepochData,
    dataHashSize,
    -- $
    AuxiliaryData (AuxiliaryData, scripts, txMD),
    AuxiliaryDataHash (..),
    -- $
    ppZerepochData,
    ppData,
    ppAuxiliaryData,
  )
where

import Bcc.Binary (FromCBOR (..), ToCBOR (..), TokenType (..), peekTokenType, withSlice)
import Bcc.Ledger.Aurum.Scripts
  ( Script (..),
    isZerepochScript,
  )
import Bcc.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Bcc.Ledger.BaseTypes (StrictMaybe (..))
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CC
import Bcc.Ledger.Era (Crypto, Era)
import Bcc.Ledger.Hashes
  ( EraIndependentAuxiliaryData,
    EraIndependentData,
  )
import Bcc.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppInteger,
    ppList,
    ppLong,
    ppMap,
    ppMetadatum,
    ppPair,
    ppSexp,
    ppStrictSeq,
    ppWord64,
  )
import Bcc.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash (..),
    hashAnnotated,
  )
import Bcc.Ledger.Serialization (mapFromCBOR)
import Bcc.Prelude (HeapWords (..), heapWords0, heapWords1)
import qualified Codec.Serialise as Cborg (Serialise (..))
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Coders
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import qualified Zerepoch.V1.Ledger.Api as Zerepoch
import Sophie.Spec.Ledger.Metadata (Metadatum)

-- =====================================================================
-- Zerepoch.Data is the type that Zerepoch expects as data.
-- It is imported from the Zerepoch package, but it needs a few additional
-- instances to also work in the ledger.

instance FromCBOR (Annotator Zerepoch.Data) where
  fromCBOR = pure <$> Cborg.decode

instance ToCBOR Zerepoch.Data where
  toCBOR = Cborg.encode

deriving anyclass instance NoThunks Zerepoch.BuiltinByteString

deriving instance NoThunks Zerepoch.Data

-- ============================================================================
-- the newtype Data is a wrapper around the type that Zerepoch expects as data.
-- The newtype will memoize the serialized bytes.

newtype Data era = DataConstr (MemoBytes Zerepoch.Data)
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (SafeToHash, ToCBOR)

instance Typeable era => FromCBOR (Annotator (Data era)) where
  fromCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice fromCBOR
    pure (Annotator (\fullbytes -> DataConstr (Memo (getT fullbytes) (toShort (toStrict (getBytes fullbytes))))))

instance (Crypto era ~ c) => HashAnnotated (Data era) EraIndependentData c

instance NoThunks (Data era)

pattern Data :: Zerepoch.Data -> Data era
pattern Data p <-
  DataConstr (Memo p _)
  where
    Data p = DataConstr (memoBytes (To p))

getZerepochData :: Data era -> Zerepoch.Data
getZerepochData (DataConstr (Memo d _)) = d

-- =============================================================================

type DataHash crypto = SafeHash crypto EraIndependentData

hashData :: Era era => Data era -> DataHash (Crypto era)
hashData = hashAnnotated

-- Size of the datum hash attached to the output (could be Nothing)
dataHashSize :: StrictMaybe (DataHash c) -> Integer
dataHashSize SNothing = 0
dataHashSize (SJust _) = 10

instance (CC.Crypto c) => HeapWords (StrictMaybe (DataHash c)) where
  heapWords SNothing = heapWords0
  heapWords (SJust a) = heapWords1 a

-- =============================================================================
-- Version without serialized bytes

data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { txMD' :: !(Map Word64 Metadatum),
    scripts' :: !(StrictSeq (Core.Script era))
  }
  deriving (Generic)

deriving instance Eq (Core.Script era) => Eq (AuxiliaryDataRaw era)

deriving instance Show (Core.Script era) => Show (AuxiliaryDataRaw era)

deriving via InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryDataRaw era) instance NoThunks (AuxiliaryDataRaw era)

instance
  ( Typeable era,
    Ord (Core.Script era),
    Core.Script era ~ Script era,
    ToCBOR (Core.Script era),
    Typeable (Crypto era)
  ) =>
  ToCBOR (AuxiliaryDataRaw era)
  where
  toCBOR (AuxiliaryDataRaw m s) =
    encode (encodeRaw m s)

encodeRaw ::
  ( Core.Script era ~ Script era,
    Typeable (Crypto era)
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
  Encode ('Closed 'Sparse) (AuxiliaryDataRaw era)
encodeRaw metadata allScripts =
  Tag 259 $
    Keyed
      (\m tss pss -> AuxiliaryDataRaw m (StrictSeq.fromList $ tss <> pss))
      !> Omit null (Key 0 $ mapEncode metadata)
      !> Omit null (Key 1 $ E (encodeFoldable . mapMaybe getTimelock) timelocks)
      !> Omit null (Key 2 $ E (encodeFoldable . mapMaybe getZerepoch) zerepochScripts)
  where
    getTimelock (TimelockScript x) = Just x
    getTimelock _ = Nothing
    getZerepoch (ZerepochScript x) = Just x
    getZerepoch _ = Nothing
    (zerepochScripts, timelocks) =
      List.partition
        isZerepochScript
        (Foldable.toList allScripts)

instance
  ( Era era,
    Ord (Core.Script era),
    FromCBOR (Annotator (Core.Script era)),
    Core.Script era ~ Script era
  ) =>
  FromCBOR (Annotator (AuxiliaryDataRaw era))
  where
  fromCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeSophie
      TypeMapLen64 -> decodeSophie
      TypeMapLenIndef -> decodeSophie
      TypeListLen -> decodeSophieMA
      TypeListLen64 -> decodeSophieMA
      TypeListLenIndef -> decodeSophieMA
      TypeTag -> decodeAurum
      TypeTag64 -> decodeAurum
      _ -> error "Failed to decode AuxiliaryData"
    where
      decodeSophie =
        decode
          ( Ann (Emit AuxiliaryDataRaw)
              <*! Ann (D mapFromCBOR)
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeSophieMA =
        decode
          ( Ann (RecD AuxiliaryDataRaw)
              <*! Ann (D mapFromCBOR)
              <*! D
                ( sequence
                    <$> decodeStrictSeq
                      (fmap TimelockScript <$> fromCBOR)
                )
          )
      decodeAurum =
        decode $
          TagD 259 $
            SparseKeyed "AuxiliaryData" (pure emptyAuxData) auxDataField []

      auxDataField :: Word -> Field (Annotator (AuxiliaryDataRaw era))
      auxDataField 0 = fieldA (\x ad -> ad {txMD' = x}) (D mapFromCBOR)
      auxDataField 1 =
        fieldAA
          (\x ad -> ad {scripts' = scripts' ad <> (TimelockScript <$> x)})
          (D (sequence <$> decodeStrictSeq fromCBOR))
      auxDataField 2 =
        fieldA
          (\x ad -> ad {scripts' = scripts' ad <> (ZerepochScript <$> x)})
          (D (decodeStrictSeq fromCBOR))
      auxDataField n = field (\_ t -> t) (Invalid n)

emptyAuxData :: AuxiliaryDataRaw era
emptyAuxData = AuxiliaryDataRaw mempty mempty

-- ================================================================================
-- Version with serialized bytes.

newtype AuxiliaryData era = AuxiliaryDataConstr (MemoBytes (AuxiliaryDataRaw era))
  deriving newtype (ToCBOR, SafeToHash)

instance (Crypto era ~ c) => HashAnnotated (AuxiliaryData era) EraIndependentAuxiliaryData c

deriving instance Eq (Core.Script era) => Eq (AuxiliaryData era)

deriving instance Show (Core.Script era) => Show (AuxiliaryData era)

deriving via InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryData era) instance NoThunks (AuxiliaryData era)

deriving via
  (Mem (AuxiliaryDataRaw era))
  instance
    ( Era era,
      Ord (Core.Script era),
      FromCBOR (Annotator (Core.Script era)),
      Script era ~ Core.Script era -- FIXME: this smells fishy
    ) =>
    FromCBOR (Annotator (AuxiliaryData era))

pattern AuxiliaryData ::
  ( Era era,
    ToCBOR (Core.Script era),
    Core.Script era ~ Script era,
    Ord (Core.Script era)
  ) =>
  Map Word64 Metadatum ->
  StrictSeq (Core.Script era) ->
  AuxiliaryData era
pattern AuxiliaryData {txMD, scripts} <-
  AuxiliaryDataConstr (Memo (AuxiliaryDataRaw txMD scripts) _)
  where
    AuxiliaryData m s =
      AuxiliaryDataConstr
        ( memoBytes
            (encodeRaw m s)
        )

{-# COMPLETE AuxiliaryData #-}

-- =======================================================

ppZerepochData :: Zerepoch.Data -> PDoc
ppZerepochData (Zerepoch.Constr tag args) = ppSexp "Constr" [ppInteger tag, ppList ppZerepochData args]
ppZerepochData (Zerepoch.Map pairs) = ppSexp "Map" [ppList (ppPair ppZerepochData ppZerepochData) pairs]
ppZerepochData (Zerepoch.List xs) = ppSexp "List" [ppList ppZerepochData xs]
ppZerepochData (Zerepoch.I i) = ppSexp "I" [ppInteger i]
ppZerepochData (Zerepoch.B bytes) = ppSexp "B" [ppLong bytes]

instance PrettyA Zerepoch.Data where prettyA = ppZerepochData

ppData :: Data era -> PDoc
ppData (DataConstr (Memo x _)) = ppSexp "Data" [ppZerepochData x]

instance PrettyA (Data era) where prettyA = ppData

ppAuxiliaryData :: (PrettyA (Core.Script era)) => AuxiliaryData era -> PDoc
ppAuxiliaryData (AuxiliaryDataConstr (Memo (AuxiliaryDataRaw m s) _)) =
  ppSexp "AuxiliaryData" [ppMap ppWord64 ppMetadatum m, ppStrictSeq prettyA s]

instance (PrettyA (Core.Script era)) => PrettyA (AuxiliaryData era) where prettyA = ppAuxiliaryData
