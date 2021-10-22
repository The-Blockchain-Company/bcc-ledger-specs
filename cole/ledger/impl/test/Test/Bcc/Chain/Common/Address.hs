{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Chain.Common.Address
  ( tests,
  )
where

import Bcc.Chain.Common (addrNetworkMagic, isRedeemAddress)
import Bcc.Prelude
import Hedgehog (cover, forAll, property, (===))
import Test.Bcc.Chain.Common.Gen (genAddress, genAddressWithNM, genNetworkMagic)
import Test.Bcc.Prelude
import Test.Options (TSGroup, TSProperty, withTestsTS)

ts_prop_addressNetworkMagicIdentity :: TSProperty
ts_prop_addressNetworkMagicIdentity =
  withTestsTS 1000 . property $ do
    nm <- forAll genNetworkMagic
    addr <- forAll (genAddressWithNM nm)
    nm === addrNetworkMagic addr

ts_prop_isRedeemAddress :: TSProperty
ts_prop_isRedeemAddress =
  withTestsTS 1000 . property $ do
    addr <- forAll genAddress
    cover 30 "Redeem Address" $ isRedeemAddress addr
    cover 30 "Pubkey Address" $ not (isRedeemAddress addr)

tests :: TSGroup
tests = $$discoverPropArg
