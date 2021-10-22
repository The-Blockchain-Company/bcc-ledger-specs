{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Ledger.Evie.ScriptTranslation
  ( testScriptPostTranslation,
  )
where

import Bcc.Ledger.Evie.Translation ()
import Bcc.Ledger.Era (TranslateEra (..))
import qualified Bcc.Ledger.Val as Val
import Bcc.Slotting.Slot (SlotNo (..))
import Control.Monad.Except (runExcept)
import Control.State.Transition.Extended (TRC (..))
import Data.Default.Class (def)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Sophie.Spec.Ledger.API as S
import Sophie.Spec.Ledger.LedgerState ()
import Sophie.Spec.Ledger.PParams (emptyPParams)
import Sophie.Spec.Ledger.Tx (hashScript, scriptWits)
import Sophie.Spec.Ledger.UTxO (txid)
import Test.Bcc.Ledger.EraBuffet
  ( EvieEra,
    SophieEra,
    StandardCrypto,
  )
import Test.Sophie.Spec.Ledger.SentryUtils (applySTSTest, runSophieBase)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

type Evie = EvieEra StandardCrypto

type Sophie = SophieEra StandardCrypto

bootstrapTxId :: S.TxId StandardCrypto
bootstrapTxId = txid @Sophie txb
  where
    txb =
      S.TxBody
        mempty
        StrictSeq.empty
        StrictSeq.empty
        (S.Wdrl mempty)
        (S.Coin 0)
        (SlotNo 0)
        S.SNothing
        S.SNothing

fromRight :: Either e a -> a
fromRight (Right x) = x
fromRight _ = undefined

script :: S.MultiSig StandardCrypto
script = S.RequireAllOf []

scriptHash :: S.ScriptHash StandardCrypto
scriptHash = hashScript @Sophie script

testScriptPostTranslation :: TestTree
testScriptPostTranslation =
  testCase
    "we should still be able to spend a translated script"
    $ let addr =
            S.Addr
              S.Testnet
              (S.ScriptHashObj scriptHash)
              S.StakeRefNull
          utxo =
            S.UTxO $
              Map.singleton
                (S.TxIn bootstrapTxId 0)
                (S.TxOut addr (Val.inject (S.Coin 1)))
          env = S.LedgerEnv (SlotNo 0) 0 emptyPParams (S.AccountState (S.Coin 0) (S.Coin 0))
          utxoStSophie = def {S._utxo = utxo}
          utxoStEvie = fromRight . runExcept $ translateEra @Evie () utxoStSophie
          txb =
            S.TxBody
              (Set.singleton $ S.TxIn bootstrapTxId 0)
              StrictSeq.empty
              StrictSeq.empty
              (S.Wdrl mempty)
              (S.Coin 1)
              (SlotNo 1)
              S.SNothing
              S.SNothing
          wits = mempty {scriptWits = Map.singleton scriptHash script}
          txs = S.Tx txb wits S.SNothing
          txa = fromRight . runExcept $ translateEra @Evie () txs
          result =
            runSophieBase $
              applySTSTest @(S.LEDGER Evie)
                (TRC (env, (utxoStEvie, def), txa))
       in case result of
            Left e -> error $ show e
            Right _ -> pure ()
