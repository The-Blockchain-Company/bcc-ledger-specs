{-# LANGUAGE OverloadedStrings #-}

module Test.Bcc.Chain.MempoolPayload.Example
  ( exampleMempoolPayload,
    exampleMempoolPayload1,
    exampleMempoolPayload2,
    exampleMempoolPayload3,
  )
where

import Bcc.Chain.MempoolPayload (AMempoolPayload (..), MempoolPayload)
import Data.List ((!!))
import Test.Bcc.Chain.Delegation.Example as Delegation
  ( exampleCertificates,
  )
import Test.Bcc.Chain.UTxO.Example (exampleTxAux)
import Test.Bcc.Chain.Update.Example as Update
  ( exampleProposal,
    exampleVote,
  )

exampleMempoolPayload :: MempoolPayload
exampleMempoolPayload = MempoolTx exampleTxAux

exampleMempoolPayload1 :: MempoolPayload
exampleMempoolPayload1 = MempoolDlg (Delegation.exampleCertificates !! 0)

exampleMempoolPayload2 :: MempoolPayload
exampleMempoolPayload2 = MempoolUpdateProposal Update.exampleProposal

exampleMempoolPayload3 :: MempoolPayload
exampleMempoolPayload3 = MempoolUpdateVote Update.exampleVote
