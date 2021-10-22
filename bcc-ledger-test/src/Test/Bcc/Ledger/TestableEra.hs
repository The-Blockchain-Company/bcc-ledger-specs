-- | Defines the requirements on an era to be testable
module Test.Bcc.Ledger.TestableEra where

import Sophie.Spec.Ledger.API

class
  ( ApplyBlock era,
    ApplyTx era,
    GetLedgerView era
  ) =>
  TestableEra era
