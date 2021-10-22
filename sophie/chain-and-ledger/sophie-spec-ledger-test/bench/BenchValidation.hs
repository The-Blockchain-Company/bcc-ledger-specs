{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module BenchValidation
  ( ValidateInput (..),
    validateInput,
    benchValidate,
    benchreValidate,
    applyBlock,
    sizes,
    updateChain,
    updateAndTickChain,
    genUpdateInputs,
  )
where

import Bcc.Ledger.BaseTypes (Globals (..))
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Crypto as CryptoClass
import Bcc.Ledger.Era (Era (..))
import Bcc.Ledger.Sophie.Constraints (TransValue)
import Bcc.Prelude (NFData (rnf))
import Bcc.Protocol.TOptimum.BHeader (BHeader (..), LastAppliedBlock (..))
import Bcc.Protocol.TOptimum.Rules.Prtcl (PrtclState (..))
import Bcc.Slotting.Slot (withOriginToMaybe)
import Control.Monad.Except ()
import Control.State.Transition (STS (State))
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import qualified Data.Map as Map
import Data.Proxy
import qualified Sophie.Spec.Ledger.API as API
import Sophie.Spec.Ledger.API.Protocol
  ( ChainDepState (..),
    ChainTransitionError,
    LedgerView (..),
    currentLedgerView,
    tickChainDepState,
    updateChainDepState,
  )
import Sophie.Spec.Ledger.Bench.Gen (genBlock, genChainState)
import Sophie.Spec.Ledger.BlockChain (Block (..), slotToNonce)
import Sophie.Spec.Ledger.EpochBoundary (unBlocksMade)
import Sophie.Spec.Ledger.LedgerState
  ( NewEpochState,
    nesBcur,
  )
import Sophie.Spec.Ledger.STS.Chain (ChainState (..))
import Sophie.Spec.Ledger.STS.Tickn (TicknState (..))
import Sophie.Spec.Ledger.TxBody (TransTxBody, TransTxId)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Sophie.Spec.Ledger.Generator.Core (GenEnv)
-- Use Another constraint, so this works in all Eras
import Test.Sophie.Spec.Ledger.Generator.EraGen (EraGen, MinLEDGER_STS)
import Test.Sophie.Spec.Ledger.Generator.Presets (genEnv)
import Test.Sophie.Spec.Ledger.Serialisation.Generators ()
import Test.Sophie.Spec.Ledger.VestedSealUtils (SophieTest, testGlobals)

data ValidateInput era = ValidateInput Globals (NewEpochState era) (Block era)

sizes :: ValidateInput era -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance NFData (ValidateInput era) where
  rnf (ValidateInput a b c) = seq a (seq b (seq c ()))

validateInput ::
  ( EraGen era,
    SophieTest era,
    Mock (Crypto era),
    Core.EraRule "LEDGERS" era ~ API.LEDGERS era,
    QC.HasTrace (API.LEDGERS era) (GenEnv era),
    API.ApplyBlock era,
    API.GetLedgerView era,
    MinLEDGER_STS era
  ) =>
  Int ->
  IO (ValidateInput era)
validateInput utxoSize = genValidateInput utxoSize

genValidateInput ::
  ( EraGen era,
    SophieTest era,
    Mock (Crypto era),
    Core.EraRule "LEDGERS" era ~ API.LEDGERS era,
    QC.HasTrace (API.LEDGERS era) (GenEnv era),
    API.ApplyBlock era,
    API.GetLedgerView era,
    MinLEDGER_STS era
  ) =>
  Int ->
  IO (ValidateInput era)
genValidateInput n = do
  let ge = genEnv (Proxy :: Proxy era)
  chainstate <- genChainState n ge
  block <- genBlock ge chainstate
  pure (ValidateInput testGlobals (chainNes chainstate) block)

benchValidate ::
  forall era.
  API.ApplyBlock era =>
  ValidateInput era ->
  IO (NewEpochState era)
benchValidate (ValidateInput globals state block) =
  case API.applyBlock @era globals state block of
    Right x -> pure x
    Left x -> error (show x)

applyBlock ::
  forall era.
  ( TransTxId Show era,
    TransTxBody NFData era,
    TransValue NFData era,
    API.ApplyBlock era,
    NFData (Core.PParams era),
    NFData (State (Core.EraRule "PPUP" era))
  ) =>
  ValidateInput era ->
  Int ->
  Int
applyBlock (ValidateInput globals state block) n =
  case API.applyBlock @era globals state block of
    Right x -> seq (rnf x) (n + 1)
    Left x -> error (show x)

benchreValidate ::
  ( API.ApplyBlock era
  ) =>
  ValidateInput era ->
  NewEpochState era
benchreValidate (ValidateInput globals state block) =
  API.reapplyBlock globals state block

-- ==============================================================

data UpdateInputs c
  = UpdateInputs
      !Globals
      !(LedgerView c)
      !(BHeader c)
      !(ChainDepState c)

instance CryptoClass.Crypto c => Show (UpdateInputs c) where
  show (UpdateInputs _globals vl bh st) =
    show vl ++ "\n" ++ show bh ++ "\n" ++ show st

instance NFData (LedgerView era) where
  rnf (LedgerView _D _extraEntropy _pool _delegs _ccd) = ()

instance CryptoClass.Crypto c => NFData (BHeader c) where
  rnf (BHeader _ _) = ()

instance NFData (ChainDepState c) where
  rnf (ChainDepState _ _ _) = ()

instance NFData Globals where
  rnf (Globals _ _ _ _ _ _ _ _ _ _ _ _) = ()

instance NFData (ChainTransitionError c) where
  rnf _ = ()

instance CryptoClass.Crypto c => NFData (UpdateInputs c) where
  rnf (UpdateInputs g lv bh st) =
    seq (rnf g) (seq (rnf lv) (seq (rnf bh) (rnf st)))

genUpdateInputs ::
  forall era.
  ( EraGen era,
    Mock (Crypto era),
    SophieTest era,
    MinLEDGER_STS era,
    API.GetLedgerView era,
    Core.EraRule "LEDGERS" era ~ API.LEDGERS era,
    QC.HasTrace (API.LEDGERS era) (GenEnv era),
    API.ApplyBlock era
  ) =>
  Int ->
  IO (UpdateInputs (Crypto era))
genUpdateInputs utxoSize = do
  let ge = genEnv (Proxy :: Proxy era)
  chainstate <- genChainState utxoSize ge
  (Block blockheader _) <- genBlock ge chainstate
  let ledgerview = currentLedgerView (chainNes chainstate)
  let (ChainState _newepochState keys eta0 etaV etaC etaH slot) = chainstate
  let prtclState = PrtclState keys eta0 etaV
  let ticknState = TicknState etaC etaH
  let nonce = case withOriginToMaybe slot of
        Just (LastAppliedBlock _blknum slotnum _hash) -> slotToNonce slotnum
        Nothing -> error "Empty Slot"
  pure
    ( UpdateInputs
        testGlobals
        ledgerview
        blockheader
        (ChainDepState prtclState ticknState nonce)
    )

updateChain ::
  (Mock c) =>
  UpdateInputs c ->
  Either (ChainTransitionError c) (ChainDepState c)
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

updateAndTickChain ::
  (Mock c) =>
  UpdateInputs c ->
  Either (ChainTransitionError c) (ChainDepState c)
updateAndTickChain (UpdateInputs gl lv bh st) =
  updateChainDepState gl lv bh
    . tickChainDepState gl lv True
    $ st
