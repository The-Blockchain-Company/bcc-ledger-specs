-- | This module exports pieces one may use to make a new Era.
--   The pieces come in 3 flavors:
--   1) Crypto
--   2) Era's
--   3) Type families
--   Some of these pieces are pre-specialized to the Sophie,
--   Evie, and Jen Eras. Some are trivial datatypes that one
--   may use as type instances for the type families when nothing
--   more specific is needed. One may make your own Era or just use
--   some of the pieces, which have been collected here for convenience.
--   To make a new Era, import this file, then write something like this:
--   data MyEra
--   instance Era MyEra where
--   type Crypto MyEra = TestCrypto
--   type instance Value MyEra = ConcreteValue.Value MyEra
--   type instance Script MyEra = TestScript
--   type instance TxBody MyEra = Jen.TxBody MyEra
module Test.Bcc.Ledger.EraBuffet
  ( TestCrypto, -- These are two crypto versions
    StandardCrypto,
    SophieEra, -- These are the crypto parameterized Eras re-exported for convenience.
    JenEra, -- one needs to apply these to a crypto be be concrete
    EvieEra,
    Value, -- These are the type families re-exported for convenience.
    Script,
    TxBody,
    AuxiliaryData,
    Era (..), -- The Era class re-exported
  )
where

import Bcc.Ledger.Evie (EvieEra)
import Bcc.Ledger.Core (AuxiliaryData, Script, TxBody, Value)
import Bcc.Ledger.Era (Crypto, Era)
import Bcc.Ledger.Jen (JenEra)
import Bcc.Ledger.Sophie (SophieEra)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (StandardCrypto, TestCrypto)

{------------------------------------------------------------------------------
 Example concrete Eras:

  type SophieTest = SophieEra TestCrypto

  type SophieStandard = SophieEra StandardCrypto

  type JenTest = JenEra TestCrypto

  type JenStandard = JenEra StandardCrypto

  type EvieTest = EvieEra TestCrypto

  type EvieStandard = EvieEra StandardCrypto
------------------------------------------------------------------------------}
