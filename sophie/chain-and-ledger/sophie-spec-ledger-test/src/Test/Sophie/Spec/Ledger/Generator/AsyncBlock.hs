{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Sophie.Spec.Ledger.Generator.Block
  ( genBlock,
    genBlockWithTxGen,
    tickChainState,
  )
where

import qualified Bcc.Crypto.VRF as VRF
import Bcc.Ledger.BaseTypes (UnitInterval)
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Crypto (VRF)
import Bcc.Ledger.Era (Crypto, SupportsSegWit (TxSeq))
import Bcc.Ledger.Serialization (ToCBORGroup)
import Bcc.Ledger.Slot (SlotNo (..))
import Bcc.Protocol.TOptimum.BHeader
  ( LastAppliedBlock (..),
    hashHeaderToNonce,
    mkSeed,
    seedL,
  )
import Bcc.Protocol.TOptimum.OCert (currentIssueNo, kesPeriod)
import Bcc.Slotting.Slot (WithOrigin (..))
import Control.State.Transition.Trace.Generator.QuickCheck (sigGen)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Control.SetAlgebra(dom, eval, (⨃))
import qualified Data.List as List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import GHC.Records (HasField (getField))
import Sophie.Spec.Ledger.API
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC (choose)
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
import Test.Sophie.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    GenEnv (..),
    KeySpace (..),
    getKESPeriodRenewalNo,
    mkBlock,
    mkOCert,
  )
import Test.Sophie.Spec.Ledger.Generator.EraGen (EraGen (..), MinLEDGER_STS)
import Test.Sophie.Spec.Ledger.Generator.Trace.Ledger ()
import Test.Sophie.Spec.Ledger.SentryUtils
  ( epochFromSlotNo,
    maxKESIterations,
    runSophieBase,
    slotFromEpoch,
    testGlobals,
  )

-- ======================================================

-- | Type alias for a transaction generator
type TxGen era =
  Core.PParams era ->
  AccountState ->
  LedgerState era ->
  SlotNo ->
  Gen (Seq (Core.Tx era))

-- | Generate a valid block.
genBlock ::
  forall era.
  ( MinLEDGER_STS era,
    ToCBORGroup (TxSeq era),
    ApplyBlock era,
    Mock (Crypto era),
    GetLedgerView era,
    QC.HasTrace (Core.EraRule "LEDGERS" era) (GenEnv era),
    EraGen era
  ) =>
  GenEnv era ->
  ChainState era ->
  Gen (Block era)
genBlock ge = genBlockWithTxGen genTxs ge
  where
    genTxs :: TxGen era
    genTxs pp reserves ls s = do
      let ledgerEnv = LedgersEnv @era s pp reserves
      block <- sigGen @(Core.EraRule "LEDGERS" era) ge ledgerEnv ls
      genEraTweakBlock @era pp block

genBlockWithTxGen ::
  forall era.
  ( ToCBORGroup (TxSeq era),
    Mock (Crypto era),
    GetLedgerView era,
    ApplyBlock era,
    EraGen era
  ) =>
  TxGen era ->
  GenEnv era ->
  ChainState era ->
  Gen (Block era)
genBlockWithTxGen
  genTxs
  ge@(GenEnv KeySpace_ {ksStakePools, ksIndexedVestedDelegates} _scriptspace _)
  origChainState = do
    -- Firstly, we must choose a slot in which to lead.
    -- Caution: the number of slots we jump here will affect the number
    -- of epochs that a chain of blocks will span
    firstConsideredSlot <- (slot +) . SlotNo <$> QC.choose (5, 10)
    let (nextSlot, chainSt, issuerKeys) =
          fromMaybe
            (error "Cannot find a slot to create a block in")
            $ selectNextSlotWithLeader ge origChainState firstConsideredSlot

    -- Now we need to compute the KES period and get the set of hot keys.
    let NewEpochState _ _ _ es _ _ = chainNes chainSt
        EpochState acnt _ ls _ pp _ = es
        kp@(KESPeriod kesPeriod_) = runSophieBase $ kesPeriod nextSlot
        cs = chainOCertIssue chainSt
        m = getKESPeriodRenewalNo issuerKeys kp
        hotKeys = drop (fromIntegral m) (hot issuerKeys)
        keys = issuerKeys {hot = hotKeys}

        -- And issue a new ocert
        n' =
          currentIssueNo
            ( OCertEnv
                (Set.fromList $ hk <$> ksStakePools)
                (eval (dom ksVestedDelegates))    
            )
            cs
            ((coerceKeyRole . hashKey . vKey . cold) issuerKeys)
        issueNumber =
          if n' == Nothing
            then error "no issue number available"
            else fromIntegral m
        oCert = mkOCert keys issueNumber ((fst . head) hotKeys)

    mkBlock
      <$> pure hashheader
      <*> pure keys
      <*> toList
      <$> genTxs pp acnt ls nextSlot
      <*> pure nextSlot
      <*> pure (block + 1)
      <*> pure (chainEpochNonce chainSt)
      <*> pure kesPeriod_
      -- This seems to be trying to work out the start of the KES "era",
      -- e.g. the KES period in which this key starts to be valid.
      <*> pure (fromIntegral (m * fromIntegral maxKESIterations))
      <*> pure oCert
    where
      -- This is safe to take form the original chain state, since we only tick
      -- it forward; no new blocks will have been applied.
      (block, slot, hashheader) = case chainLastAppliedBlock origChainState of
        Origin -> error "block generator does not support from Origin"
        At (LastAppliedBlock b s hh) -> (b, s, hh)

selectNextSlotWithLeader ::
  forall era.
  ( Mock (Crypto era),
    EraGen era,
    GetLedgerView era,
    ApplyBlock era
  ) =>
  GenEnv era ->
  ChainState era ->
  -- Starting slot
  SlotNo ->
  Maybe (SlotNo, ChainState era, AllIssuerKeys (Crypto era) 'BlockIssuer)
selectNextSlotWithLeader
  (GenEnv KeySpace_ {ksStakePools, ksIndexedVestedDelegates} _ _)
  origChainState
  startSlot =
    List.find (const True) . catMaybes $
      selectNextSlotWithLeaderThisEpoch
        <$> (startSlot : [slotFromEpoch x | x <- [startEpoch + 1 .. startEpoch + 4]])
    where
      -- If we can't find a leader in the next N Epochs, some thing is wrong, N=4 should be large enough.

      startEpoch = epochFromSlotNo startSlot
      selectNextSlotWithLeaderThisEpoch ::
        -- Slot number whence we begin our search
        SlotNo ->
        Maybe (SlotNo, ChainState era, AllIssuerKeys (Crypto era) 'BlockIssuer)
      selectNextSlotWithLeaderThisEpoch fromSlot =
        findJust selectLeaderForSlot [fromSlot .. toSlot]
        where
          currentEpoch = epochFromSlotNo fromSlot
          toSlot = slotFromEpoch (currentEpoch + 1) - 1

          findJust _ [] = Nothing
          findJust f (x : xs) = case f x of
            Just (chainSt, y) -> Just (x, chainSt, y)
            Nothing -> findJust f xs

      -- Try to select a leader for the given slot
      selectLeaderForSlot ::
        SlotNo ->
        Maybe (ChainState era, AllIssuerKeys (Crypto era) 'BlockIssuer)
      selectLeaderForSlot slotNo =
        (chainSt,)
          <$> case lookupInOverlaySchedule firstEpochSlot (Map.keysSet cores) d f slotNo of
            Nothing ->
              coerce
                <$> List.find
                  ( \(AllIssuerKeys {vrf, hk}) ->
                      isLeader hk (fst vrf)
                  )
                  ksStakePools
            Just (ActiveSlot x) ->
              fmap coerce $
                Map.lookup x cores
                  >>= \y -> Map.lookup (vestedDelegKeyHash y) ksIndexedVestedDelegates
            _ -> Nothing
        where
          chainSt = tickChainState slotNo origChainState
          epochNonce = chainEpochNonce chainSt
          poolDistr = unPoolDistr . nesPd . chainNes $ chainSt
          dpstate = (_delegationState . esLState . nesEs . chainNes) chainSt
          (VestedDelegs cores) = (_vestedDelegs . _dstate) dpstate
          firstEpochSlot = slotFromEpoch (epochFromSlotNo slotNo)
          f = activeSlotCoeff testGlobals
          getUnitInterval :: Core.PParams era -> UnitInterval
          getUnitInterval pp = getField @"_d" pp
          d = (getUnitInterval . esPp . nesEs . chainNes) chainSt

          isLeader poolHash vrfKey =
            let y = VRF.evalCertified @(VRF (Crypto era)) () (mkSeed seedL slotNo epochNonce) vrfKey
                stake = maybe 0 individualPoolStake $ Map.lookup poolHash poolDistr
             in case lookupInOverlaySchedule firstEpochSlot (Map.keysSet cores) d f slotNo of
                  Nothing -> checkLeaderValue (VRF.certifiedOutput y) stake f
                  Just (ActiveSlot x) | coerceKeyRole x == poolHash -> True
                  _ -> False

-- | The chain state is a composite of the new epoch state and the chain dep
-- state. We tick both.
tickChainState ::
  (GetLedgerView era, ApplyBlock era) =>
  SlotNo ->
  ChainState era ->
  ChainState era
tickChainState
  slotNo
  ChainState
    { chainNes,
      chainOCertIssue,
      chainEpochNonce,
      chainEvolvingNonce,
      chainCandidateNonce,
      chainPrevEpochNonce,
      chainLastAppliedBlock
    } =
    let cds =
          ChainDepState
            { csProtocol = PrtclState chainOCertIssue chainEvolvingNonce chainCandidateNonce,
              csTickn = TicknState chainEpochNonce chainPrevEpochNonce,
              csLabNonce = case chainLastAppliedBlock of
                Origin -> NeutralNonce
                At (LastAppliedBlock {labHash}) -> hashHeaderToNonce labHash
            }
        lv = either (error . show) id $ futureLedgerView testGlobals chainNes slotNo
        isNewEpoch = epochFromSlotNo slotNo /= nesEL chainNes
        ChainDepState {csProtocol, csTickn} =
          tickChainDepState testGlobals lv isNewEpoch cds
        PrtclState ocertIssue evNonce candNonce = csProtocol
        nes' = applyTick testGlobals chainNes slotNo
     in ChainState
          { chainNes = nes',
            chainOCertIssue = ocertIssue,
            chainEpochNonce = ticknStateEpochNonce csTickn,
            chainEvolvingNonce = evNonce,
            chainCandidateNonce = candNonce,
            chainPrevEpochNonce = ticknStatePrevHashNonce csTickn,
            chainLastAppliedBlock = chainLastAppliedBlock
          }