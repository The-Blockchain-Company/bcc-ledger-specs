{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
where

import Bcc.Binary (FromCBOR, ToCBOR)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import Bcc.Ledger.Hashes (EraIndependentAuxiliaryData)
import Bcc.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData (..))
import NoThunks.Class (NoThunks (..))

newtype AuxiliaryDataHash crypto = AuxiliaryDataHash
  { unsafeAuxiliaryDataHash :: SafeHash crypto EraIndependentAuxiliaryData
  }
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving instance
  CC.Crypto crypto =>
  ToCBOR (AuxiliaryDataHash crypto)

deriving instance
  CC.Crypto crypto =>
  FromCBOR (AuxiliaryDataHash crypto)

class ValidateAuxiliaryData era c | era -> c where
  hashAuxiliaryData :: Core.AuxiliaryData era -> AuxiliaryDataHash c
  validateAuxiliaryData :: Core.AuxiliaryData era -> Bool
