{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cole.Spec.Ledger.UTxO where

import Cole.Spec.Ledger.Core hiding ((<|))
import Cole.Spec.Ledger.Update (FactorA (..), FactorB (..), PParams (PParams), _factorA, _factorB)
import Control.Monad (replicateM)
import Data.AbstractSize (HasTypeReps, abstractSize)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (typeOf)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Test.Shepard
  ( AddShrinks (..),
    GeneOps (..),
    Shepard (..),
    SeedShepard (..),
    TinkerM,
    saveInBagOfTricks,
    tinkerRummagedOrConjureOrSave,
    (<$$>),
  )
import Test.Shepard.TH (deriveAddShrinks, deriveShepard, deriveSeedShepard)

-- | A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId {getTxId :: Hash}
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, NoThunks)
  deriving anyclass (HasTypeReps)

-- | The input of a UTxO.
--
--      * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn = TxIn TxId Natural
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable,
      HasTypeReps,
      Data,
      Typeable,
      NoThunks
    )

-- | The output of a UTxO.
data TxOut = TxOut
  { addr :: Addr,
    value :: Entropic
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable,
      HasTypeReps,
      Data,
      Typeable,
      NoThunks
    )

-- | The unspent transaction outputs.
newtype UTxO = UTxO
  { unUTxO :: Map TxIn TxOut
  }
  deriving stock (Show, Data, Typeable)
  deriving newtype (Eq, Relation, Semigroup, Monoid, NoThunks)

-- | Apply function uniformly across all outputs
mapUTxOValues :: (Entropic -> Entropic) -> UTxO -> UTxO
mapUTxOValues f (UTxO utxo) = UTxO (f' <$> utxo)
  where
    f' :: TxOut -> TxOut
    f' (TxOut addr value) = TxOut addr (f value)

addValue :: TxOut -> Entropic -> TxOut
addValue tx@TxOut {value} d = tx {value = value + d}

-- | Construct a UTxO from initial TxOuts
fromTxOuts :: [TxOut] -> UTxO
fromTxOuts = UTxO . Map.fromList . fmap (\out -> (TxIn (mkId out) 0, out))
  where
    mkId = TxId . hash . addr

-- | A raw transaction
data TxBody = TxBody
  { inputs :: [TxIn],
    outputs :: [TxOut]
  }
  deriving
    ( Eq,
      Show,
      Ord,
      Generic,
      Hashable,
      HasTypeReps,
      Data,
      Typeable,
      NoThunks
    )

txid :: TxBody -> TxId
txid = TxId . hash

-- | Total value of a transaction.
txValue :: TxBody -> Entropic
txValue TxBody {outputs} = sum $ fmap value outputs

-- | Compute the UTxO inputs of a transaction.
txins :: TxBody -> [TxIn]
txins = inputs

-- | Compute the UTxO outputs of a transaction.
txouts :: TxBody -> UTxO
txouts tx =
  UTxO $
    Map.fromList
      [(TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..]]
  where
    transId = txid tx

-- | Determine the total balance contained in the UTxO.
balance :: UTxO -> Entropic
balance (UTxO utxo) = Map.foldl' addValues mempty utxo
  where
    addValues b (TxOut _ a) = b <> a

instance Cole.Spec.Ledger.Core.HasHash TxBody where
  hash = Hash . Just . H.hash

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

pcMinFee :: PParams -> Tx -> Entropic
pcMinFee PParams {_factorA = FactorA a, _factorB = FactorB b} tx =
  fromIntegral $ a + b * txsize tx

txsize :: Tx -> Int
txsize = abstractSize costs
  where
    costs =
      Map.fromList
        [ (typeOf (undefined :: TxIn), 1),
          (typeOf (undefined :: TxOut), 1),
          (typeOf (undefined :: Wit), 1)
        ]

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

-- | Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey (Sig TxBody)
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable,
      HasTypeReps,
      Data,
      Typeable,
      NoThunks
    )

-- | A fully formed transaction.
data Tx = Tx
  { body :: TxBody,
    witnesses :: [Wit]
  }
  deriving
    ( Show,
      Eq,
      Generic,
      Hashable,
      HasTypeReps,
      Data,
      Typeable,
      NoThunks
    )

instance HasHash [Tx] where
  hash = Hash . Just . H.hash

-- | Create a witness for transaction
makeWitness :: KeyPair -> TxBody -> Wit
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

makeTxWits :: UTxO -> TxBody -> Tx
makeTxWits (UTxO utxo) tx =
  Tx
    { body = tx,
      witnesses = wits
    }
  where
    getKey txin =
      let TxOut (Addr (VKey o)) _ =
            fromMaybe
              (error "makeTxWits: Missing output for transaction input")
              $ Map.lookup txin utxo
       in KeyPair (SKey o) (VKey o)
    keys = getKey <$> inputs tx
    wits = makeWitness <$> keys <*> pure tx

--------------------------------------------------------------------------------
-- Shepards instances
--------------------------------------------------------------------------------

deriveShepard ''TxIn
deriveShepard ''TxOut
deriveShepard ''Tx
deriveShepard ''Wit
deriveShepard ''TxId

instance GeneOps s => Shepard s TxBody where
  tinker gen = do
    fIs <- fillEmptyList
    fOs <- fillEmptyList
    is <-
      tinkerRummagedOrConjureOrSave
        ( fIs
            <$$> (tinker ((\(TxBody x _) -> x) <$> gen))
        )
    os <-
      tinkerRummagedOrConjureOrSave
        ( fOs
            <$$> (tinker ((\(TxBody _ x) -> x) <$> gen))
        )
    tinkerRummagedOrConjureOrSave
      (pure (TxBody <$> is <*> os))
    where
      -- This function will insert a conjured value to an empty list. We can
      -- thus use it to ensure that the `txIns` and `txOuts` will never be
      -- empty.
      fillEmptyList :: Shepard s a => TinkerM s ([a] -> [a])
      fillEmptyList = do
        v <- conjure
        pure
          ( \xs -> case xs of
              [] -> [v]
              _ -> xs
          )

  conjure =
    saveInBagOfTricks =<< do
      -- Ensure that these lists are never empty.
      listLenI <- (+ 1) <$> transcribeGenesAsInt 15
      listLenO <- (+ 1) <$> transcribeGenesAsInt 15
      inputs <- replicateM listLenI conjure
      outputs <- replicateM listLenO conjure
      pure (TxBody inputs outputs)

--------------------------------------------------------------------------------
-- AddShrinks instances
--------------------------------------------------------------------------------

deriveAddShrinks ''TxBody
deriveAddShrinks ''TxId
deriveAddShrinks ''TxIn
deriveAddShrinks ''TxOut
deriveAddShrinks ''Tx
deriveAddShrinks ''Wit

--------------------------------------------------------------------------------
-- SeedShepard instances
--------------------------------------------------------------------------------

deriveSeedShepard ''UTxO
deriveSeedShepard ''TxId
deriveSeedShepard ''TxIn
deriveSeedShepard ''TxOut
