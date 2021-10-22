{ roots =
  [ "^Sophie.Spec.Ledger.API.*$"
  , "^Bcc.Chain.Cole.API.*$"
  , "^Main.main$"
  , "^Tests.main$"
    -- The following modules use some TH discovery, so we add them as explicit
    -- roots.
  , "^Test.Bcc.Chain.*$"
  , "^Test.Bcc.Crypto.*$"

  , "^Test.Sophie.Spec.Ledger.*$"
    -- Called in shardagnostic-node
  , "^Sophie.Spec.Ledger.Genesis.*"
  , "^Sophie.Spec.Ledger.TxData.TxOut$"
  , "^Sophie.Spec.Ledger.Scripts.hashAnyScript$"

    -- Things we're not interested in
  , "^Test.Shepard.*"

    -- Testing stuff we want to keep
  , "Test.Cole.Spec.Ledger.UTxO.Properties.tracesAreClassified"
  ]
, type-class-roots = True
}
