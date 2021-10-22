module Test.Bcc.Chain.MempoolPayload.Gen
  ( genMempoolPayload,
  )
where

import Bcc.Chain.MempoolPayload (AMempoolPayload (..), MempoolPayload)
import Bcc.Crypto (ProtocolMagicId)
import Bcc.Prelude
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Bcc.Chain.Delegation.Gen as Delegation (genCertificate)
import Test.Bcc.Chain.UTxO.Gen (genTxAux)
import Test.Bcc.Chain.Update.Gen as Update (genProposal, genVote)

genMempoolPayload :: ProtocolMagicId -> Gen MempoolPayload
genMempoolPayload pmi =
  Gen.choice
    [ MempoolTx <$> genTxAux pmi,
      MempoolDlg <$> Delegation.genCertificate pmi,
      MempoolUpdateProposal <$> Update.genProposal pmi,
      MempoolUpdateVote <$> Update.genVote pmi
    ]
