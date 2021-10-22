{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Ledger.Aurum.Rules.Utxow where

import Bcc.Binary (FromCBOR (..), ToCBOR (..))
import Bcc.Ledger.Address (Addr (..), bootstrapKeyHash, getRwdCred)
import Bcc.Ledger.Aurum.Data (DataHash)
import Bcc.Ledger.Aurum.Language (Language (..))
import Bcc.Ledger.Aurum.PParams (PParams)
import Bcc.Ledger.Aurum.ZerepochScriptApi (language, scriptsNeeded)
import Bcc.Ledger.Aurum.Rules.Utxo (AurumUTXO)
import qualified Bcc.Ledger.Aurum.Rules.Utxo as Aurum (UtxoEvent, UtxoPredicateFailure)
import Bcc.Ledger.Aurum.Scripts (Script (..))
import Bcc.Ledger.Aurum.Tx
  ( ScriptPurpose,
    ValidatedTx (..),
    hashScriptIntegrity,
    isTwoPhaseScriptAddress,
    rdptr,
  )
import Bcc.Ledger.Aurum.TxBody (ScriptIntegrityHash)
import Bcc.Ledger.Aurum.TxWitness
  ( RdmrPtr,
    TxWitness (..),
    unRedeemers,
    unTxDats,
  )
import Bcc.Ledger.BaseTypes
  ( SophieBase,
    StrictMaybe (..),
    strictMaybeToMaybe,
  )
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Credential (Credential (KeyHashObj))
import Bcc.Ledger.Era (Crypto, Era, ValidateScript (..))
import Bcc.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..), asWitness)
import Bcc.Ledger.Rules.ValidationMode ((?!#))
import Control.Iterate.SetAlgebra (domain, eval, (⊆), (◁), (➖))
import Control.State.Transition.Extended
import Data.Coders
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class
import Sophie.Spec.Ledger.Delegation.Certificates
  ( delegCWitness,
    genesisCWitness,
    poolCWitness,
    vestedCWitness,
    requiresVKeyWitness,
  )
import Sophie.Spec.Ledger.LedgerState
  ( UTxOState (..),
    WitHashes (..),
    propWits,
    unWitHashes,
    witsFromTxWitnesses,
  )
import Sophie.Spec.Ledger.PParams (Update)
import Sophie.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Sophie.Spec.Ledger.STS.Utxow
  ( SophieStyleWitnessNeeds,
    UtxowEvent (UtxoEvent),
    UtxowPredicateFailure (..),
    sophieStyleWitness,
  )
import Sophie.Spec.Ledger.Scripts (ScriptHash (..))
import Sophie.Spec.Ledger.Tx (TxIn (..), extractKeyHashWitnessSet)
import Sophie.Spec.Ledger.TxBody
  ( DCert (DCertDeleg, DCertGenesis, DCertVested, DCertPool),
    PoolCert (RegPool),
    PoolParams (..),
    Wdrl,
    unWdrl,
  )
import Sophie.Spec.Ledger.UTxO (UTxO (..), txinLookup)

-- =================================================

-- | The Predicate failure type in the Aurum Era. It embeds the Predicate
--   failure type of the Sophie Era, as they share some failure modes.
data AurumPredFail era
  = WrappedSophieEraFailure !(UtxowPredicateFailure era)
  | MissingRedeemers ![(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
  | MissingRequiredDatums
      !(Set (DataHash (Crypto era))) -- Set of missing data hashes
      !(Set (DataHash (Crypto era))) -- Set of received data hashes
  | NonOutputSupplimentaryDatums
      !(Set (DataHash (Crypto era))) -- Set of unallowed data hashes
      !(Set (DataHash (Crypto era))) -- Set of acceptable supplimental data hashes
  | PPViewHashesDontMatch
      !(StrictMaybe (ScriptIntegrityHash (Crypto era)))
      -- ^ The PPHash in the TxBody
      !(StrictMaybe (ScriptIntegrityHash (Crypto era)))
      -- ^ Computed from the current Protocol Parameters
  | MissingRequiredSigners (Set (KeyHash 'Witness (Crypto era)))
  | UnspendableUTxONoDatumHash (Set (TxIn (Crypto era)))
  | ExtraRedeemers ![RdmrPtr]
  deriving (Generic)

deriving instance
  ( Era era,
    Show (PredicateFailure (Core.EraRule "UTXO" era)), -- The Sophie UtxowPredicateFailure needs this to Show
    Show (Core.Script era)
  ) =>
  Show (AurumPredFail era)

deriving instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "UTXO" era)), -- The Sophie UtxowPredicateFailure needs this to Eq
    Eq (Core.Script era)
  ) =>
  Eq (AurumPredFail era)

instance
  ( Era era,
    NoThunks (Core.Script era),
    NoThunks (PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  NoThunks (AurumPredFail era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.AuxiliaryData era),
    Typeable (Core.Script era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (AurumPredFail era)
  where
  toCBOR x = encode (encodePredFail x)

newtype AurumEvent era
  = WrappedSophieEraEvent (UtxowEvent era)

encodePredFail ::
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  AurumPredFail era ->
  Encode 'Open (AurumPredFail era)
encodePredFail (WrappedSophieEraFailure x) = Sum WrappedSophieEraFailure 0 !> E toCBOR x
encodePredFail (MissingRedeemers x) = Sum MissingRedeemers 1 !> To x
encodePredFail (MissingRequiredDatums x y) = Sum MissingRequiredDatums 2 !> To x !> To y
encodePredFail (NonOutputSupplimentaryDatums x y) = Sum NonOutputSupplimentaryDatums 3 !> To x !> To y
encodePredFail (PPViewHashesDontMatch x y) = Sum PPViewHashesDontMatch 4 !> To x !> To y
encodePredFail (MissingRequiredSigners x) = Sum MissingRequiredSigners 5 !> To x
encodePredFail (UnspendableUTxONoDatumHash x) = Sum UnspendableUTxONoDatumHash 6 !> To x
encodePredFail (ExtraRedeemers x) = Sum ExtraRedeemers 7 !> To x

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  FromCBOR (AurumPredFail era)
  where
  fromCBOR = decode (Summands "(AurumPredFail" decodePredFail)

decodePredFail ::
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)), -- TODO, we should be able to get rid of this constraint
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  Word ->
  Decode 'Open (AurumPredFail era)
decodePredFail 0 = SumD WrappedSophieEraFailure <! D fromCBOR
decodePredFail 1 = SumD MissingRedeemers <! From
decodePredFail 2 = SumD MissingRequiredDatums <! From <! From
decodePredFail 3 = SumD NonOutputSupplimentaryDatums <! From <! From
decodePredFail 4 = SumD PPViewHashesDontMatch <! From <! From
decodePredFail 5 = SumD MissingRequiredSigners <! From
decodePredFail 6 = SumD UnspendableUTxONoDatumHash <! From
decodePredFail 7 = SumD ExtraRedeemers <! From
decodePredFail n = Invalid n

-- =============================================

-- | given the "txscripts" field of the Witnesses, compute the set of languages used in a transaction
langsUsed :: forall era. (Core.Script era ~ Script era, ValidateScript era) => Map.Map (ScriptHash (Crypto era)) (Script era) -> Set Language
langsUsed hashScriptMap =
  Set.fromList
    [ l | (_hash, script) <- Map.toList hashScriptMap, (not . isNativeScript @era) script, Just l <- [language @era script]
    ]

{- Defined in the Sophie Utxow rule.
type SophieStyleWitnessNeeds era =
  ( HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "addrWits" (Core.Tx era) (Set (WitVKey 'Witness (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    ValidateAuxiliaryData era (Crypto era),
    ValidateScript era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  )
-}

-- | Constraints to make an Aurum Utxow STS instance
--   (in addition to SophieStyleWitnessNeeds)
type AurumStyleAdditions era =
  ( HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))), -- BE SURE AND ADD THESE INSTANCES
    HasField "scriptIntegrityHash" (Core.TxBody era) (StrictMaybe (ScriptIntegrityHash (Crypto era)))
  )

-- | A somewhat generic STS transitionRule function for the Aurum Era.
aurumStyleWitness ::
  forall era utxow.
  ( Era era,
    -- Fix some Core types to the Aurum Era
    Core.Tx era ~ ValidatedTx era, -- scriptsNeeded, checkScriptData etc. are fixed at Aurum.Tx
    Core.PParams era ~ PParams era,
    Core.Script era ~ Script era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (utxow era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    -- Asumptions needed since we are going to fix utxow when we use this in an STS Era
    BaseM (utxow era) ~ SophieBase,
    Environment (utxow era) ~ UtxoEnv era,
    State (utxow era) ~ UTxOState era,
    Signal (utxow era) ~ ValidatedTx era,
    PredicateFailure (utxow era) ~ AurumPredFail era,
    STS (utxow era),
    -- Supply the HasField and Validate instances for Aurum
    SophieStyleWitnessNeeds era,
    AurumStyleAdditions era,
    -- New transaction body fields needed for Aurum
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  TransitionRule (utxow era)
aurumStyleWitness = do
  (TRC (UtxoEnv _slot pp _stakepools _genDelegs, u', tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u'
      txbody = getField @"body" (tx :: Core.Tx era)
      witsKeyHashes = unWitHashes $ witsFromTxWitnesses @era tx

  {-  { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isNonNativeScriptAddress tx a} = dom(txdats txw)   -}
  let inputs = getField @"inputs" txbody :: (Set (TxIn (Crypto era)))
      smallUtxo = eval (inputs ◁ utxo) :: Map.Map (TxIn (Crypto era)) (Core.TxOut era)
      twoPhaseOuts =
        [ output
          | (_input, output) <- Map.toList smallUtxo,
            isTwoPhaseScriptAddress @era tx (getField @"address" output)
        ]
      utxoHashes' = mapM (getField @"datahash") twoPhaseOuts
  case utxoHashes' of
    SNothing ->
      -- In the spec, the Nothing value can end up on the left hand side
      -- of the equality check, but we must explicitly rule it out.
      failBecause . UnspendableUTxONoDatumHash . Set.fromList $
        [ input
          | (input, output) <- Map.toList smallUtxo,
            SNothing <- [getField @"datahash" output],
            isTwoPhaseScriptAddress @era tx (getField @"address" output)
        ]
    SJust utxoHashes -> do
      let txHashes = domain (unTxDats . txdats . wits $ tx)
          inputHashes = Set.fromList utxoHashes
          unmatchedDatumHashes = eval (inputHashes ➖ txHashes)
      Set.null unmatchedDatumHashes ?! MissingRequiredDatums unmatchedDatumHashes txHashes

      -- Check that all supplimental datums contained in the witness set appear in the outputs.
      let outputDatumHashes =
            Set.fromList
              [ dh
                | out <- toList $ getField @"outputs" txbody,
                  SJust dh <- [getField @"datahash" out]
              ]
          supplimentalDatumHashes = eval (txHashes ➖ inputHashes)
          (okSupplimentalDHs, notOkSupplimentalDHs) =
            Set.partition (`Set.member` outputDatumHashes) supplimentalDatumHashes
      Set.null notOkSupplimentalDHs
        ?! NonOutputSupplimentaryDatums notOkSupplimentalDHs okSupplimentalDHs

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h \mapsto s ∈ txscripts txw, s ∈ Scriptph2}     -}
  let redeemersNeeded =
        [ (rp, (sp, sh))
          | (sp, sh) <- scriptsNeeded utxo tx,
            SJust rp <- [rdptr @era txbody sp],
            Just script <- [Map.lookup sh (getField @"scriptWits" tx)],
            (not . isNativeScript @era) script
        ]
      (extraRdmrs, missingRdmrs) =
        extSymmetricDifference
          (Map.keys $ unRedeemers $ txrdmrs $ wits tx)
          id
          redeemersNeeded
          fst
  null extraRdmrs ?! ExtraRedeemers extraRdmrs
  null missingRdmrs ?! MissingRedeemers (map snd missingRdmrs)

  {-  THIS DOES NOT APPPEAR IN THE SPEC as a separate check, but
      witsVKeyNeeded includes the reqSignerHashes in the union   -}
  let reqSignerHashes' = getField @"reqSignerHashes" txbody
  eval (reqSignerHashes' ⊆ witsKeyHashes)
    ?!# MissingRequiredSigners (eval $ reqSignerHashes' ➖ witsKeyHashes)

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  let languages =
        [ l
          | (_hash, script) <- Map.toList (getField @"scriptWits" tx),
            (not . isNativeScript @era) script,
            Just l <- [language @era script]
        ]
      computedPPhash = hashScriptIntegrity pp (Set.fromList languages) (txrdmrs . wits $ tx) (txdats . wits $ tx)
      bodyPPhash = getField @"scriptIntegrityHash" txbody
  bodyPPhash == computedPPhash ?! PPViewHashesDontMatch bodyPPhash computedPPhash

  {- The sophieStyleWitness calls the UTXO rule which applies all these rules -}
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
  {-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  vestedSig := {hashKey akey | akey ∈ dom(vestedDelegs)} n witsKeyHashes -}
  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  sophieStyleWitness witsVKeyNeeded WrappedSophieEraFailure

-- | Collect the set of hashes of keys that needs to sign a given transaction.
--  This set consists of the txin owners, certificate authors, and withdrawal
--  reward accounts.
--
--  Compared to pre-Aurum eras, we additionally gather the certificates
--  required to authenticate collateral witnesses.
witsVKeyNeeded ::
  forall era tx.
  ( Era era,
    HasField "body" tx (Core.TxBody era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "address" (Core.TxOut era) (Addr (Crypto era))
  ) =>
  UTxO era ->
  tx ->
  GenDelegs (Crypto era) ->
  WitHashes (Crypto era)
witsVKeyNeeded utxo' tx genDelegs =
  WitHashes $
    certAuthors
      `Set.union` inputAuthors
      `Set.union` owners
      `Set.union` wdrlAuthors
      `Set.union` updateKeys
  where
    txbody = getField @"body" tx
    inputAuthors :: Set (KeyHash 'Witness (Crypto era))
    inputAuthors =
      foldr
        accum
        Set.empty
        ( getField @"inputs" txbody
            `Set.union` getField @"collateral" txbody
        )
      where
        accum txin ans =
          case txinLookup txin utxo' of
            Just out ->
              case getField @"address" out of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans

    wdrlAuthors :: Set (KeyHash 'Witness (Crypto era))
    wdrlAuthors = Map.foldrWithKey accum Set.empty (unWdrl (getField @"wdrls" txbody))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness (Crypto era))
    owners = foldr accum Set.empty (getField @"certs" txbody)
      where
        accum (DCertPool (RegPool pool)) ans =
          Set.union
            (Set.map asWitness (_poolOwners pool))
            ans
        accum _cert ans = ans
    cwitness (DCertDeleg dc) = extractKeyHashWitnessSet [delegCWitness dc]
    cwitness (DCertPool pc) = extractKeyHashWitnessSet [poolCWitness pc]
    cwitness (DCertGenesis gc) = Set.singleton (asWitness $ genesisCWitness gc)
    cwitness (DCertVested ac) = Set.singleton (asWitness $ vestedCWitness ac)
    cwitness c = error $ show c ++ " does not have a witness"
    -- key reg requires no witness but this is already filtered outby requiresVKeyWitness
    -- before the call to `cwitness`, so this error should never be reached.

    certAuthors :: Set (KeyHash 'Witness (Crypto era))
    certAuthors = foldr accum Set.empty (getField @"certs" txbody)
      where
        accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
        accum _cert ans = ans
    updateKeys :: Set (KeyHash 'Witness (Crypto era))
    updateKeys =
      asWitness
        `Set.map` propWits
          ( strictMaybeToMaybe $
              getField @"update" txbody
          )
          genDelegs

extSymmetricDifference :: (Ord k) => [a] -> (a -> k) -> [b] -> (b -> k) -> ([a], [b])
extSymmetricDifference as fa bs fb = (extraA, extraB)
  where
    intersection = Set.fromList (map fa as) `Set.intersection` Set.fromList (map fb bs)
    extraA = filter (\x -> not $ fa x `Set.member` intersection) as
    extraB = filter (\x -> not $ fb x `Set.member` intersection) bs

-- ====================================
-- Make the STS instance

data AurumUTXOW era

instance
  forall era.
  ( -- Fix some Core types to the Aurum Era
    Core.Tx era ~ ValidatedTx era,
    Core.PParams era ~ PParams era,
    Core.Script era ~ Script era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (AurumUTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    -- New transaction body fields needed for Aurum
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    -- Supply the HasField and Validate instances for Aurum
    SophieStyleWitnessNeeds era, -- supplies a subset of those needed. All the old Sophie Needs still apply.
    Show (Core.TxOut era),
    AurumStyleAdditions era
  ) =>
  STS (AurumUTXOW era)
  where
  type State (AurumUTXOW era) = UTxOState era
  type Signal (AurumUTXOW era) = ValidatedTx era
  type Environment (AurumUTXOW era) = UtxoEnv era
  type BaseM (AurumUTXOW era) = SophieBase
  type PredicateFailure (AurumUTXOW era) = AurumPredFail era
  type Event (AurumUTXOW era) = AurumEvent era
  transitionRules = [aurumStyleWitness]
  initialRules = []

instance
  ( Era era,
    STS (AurumUTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ Aurum.UtxoPredicateFailure era,
    Event (Core.EraRule "UTXO" era) ~ Aurum.UtxoEvent era,
    BaseM (AurumUTXOW era) ~ SophieBase,
    PredicateFailure (AurumUTXOW era) ~ AurumPredFail era,
    Event (AurumUTXOW era) ~ AurumEvent era
  ) =>
  Embed (AurumUTXO era) (AurumUTXOW era)
  where
  wrapFailed = WrappedSophieEraFailure . UtxoFailure
  wrapEvent = WrappedSophieEraEvent . UtxoEvent
