{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sophie.Spec.Ledger.Serialisation.Generators.Bootstrap
  ( genBootstrapAddress,
    genSignature,
  )
where

import qualified Bcc.Crypto.DSIGN as DSIGN
import Bcc.Ledger.Address
  ( BootstrapAddress (..),
  )
import Data.Maybe (fromJust)
import qualified Test.Bcc.Chain.Common.Gen as Cole
import Test.Bcc.Prelude (genBytes)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Hedgehog (hedgehog)

genSignature :: forall a b. (DSIGN.DSIGNAlgorithm a) => Gen (DSIGN.SignedDSIGN a b)
genSignature =
  DSIGN.SignedDSIGN
    . fromJust
    . DSIGN.rawDeserialiseSigDSIGN
    <$> hedgehog (genBytes . fromIntegral $ DSIGN.sizeSigDSIGN ([] @a))

genBootstrapAddress :: Gen (BootstrapAddress crypto)
genBootstrapAddress = BootstrapAddress <$> hedgehog Cole.genAddress
