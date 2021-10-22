{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |  AurumEra instances for EraGen and ScriptClass
module Test.Bcc.Ledger.Aurum.AurumEraGen where

import Bcc.Binary (ToCBOR (toCBOR), serializeEncoding')
import Bcc.Ledger.Address (Addr (..))
import Bcc.Ledger.Aurum (AurumEra)
import Bcc.Ledger.Aurum.Data as Aurum (AuxiliaryData (..), Data (..), DataHash)
import Bcc.Ledger.Aurum.Language (Language (ZerepochV1))
import Bcc.Ledger.Aurum.PParams (PParams' (..))
import qualified Bcc.Ledger.Aurum.PParams as Aurum (PParams, extendPP, retractPP)
import Bcc.Ledger.Aurum.ZerepochScriptApi (scriptsNeededFromBody)
import Bcc.Ledger.Aurum.Rules.Utxo (utxoEntrySize)
import Bcc.Ledger.Aurum.Rules.Utxow (langsUsed)
import Bcc.Ledger.Aurum.Scripts (isZerepochScript, pointWiseExUnits, txscriptfee)
import Bcc.Ledger.Aurum.Scripts as Aurum
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    alwaysFails,
    alwaysSucceeds,
  )
import Bcc.Ledger.Aurum.Tx
  ( IsValid (..),
    ScriptPurpose (..),
    ValidatedTx (..),
    hashScriptIntegrity,
    minfee,
    rdptr,
    totExUnits,
  )
import Bcc.Ledger.Aurum.TxBody (TxBody (..), TxOut (..), inputs')
import Bcc.Ledger.Aurum.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..), TxWitness (..))
import Bcc.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Bcc.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Credential (Credential (..))
import qualified Bcc.Ledger.Crypto as CC
import Bcc.Ledger.Era (Crypto, Era (..), ValidateScript (..))
import Bcc.Ledger.Hashes (ScriptHash)
import Bcc.Ledger.Keys (KeyHash, KeyRole (Witness))
import Bcc.Ledger.Jen (JenEra)
import Bcc.Ledger.Jen.Value (AssetName (..), PolicyID (..), Value, policies, valueFromList)
import Bcc.Ledger.SophieMA.AuxiliaryData as Jen (pattern AuxiliaryData)
import Bcc.Ledger.SophieMA.Timelocks (Timelock (..))
import Bcc.Ledger.Val (Val (coin), bccOnly, (<+>), (<×>))
import Bcc.Slotting.Slot (SlotNo (..))
import Control.Iterate.SetAlgebra (eval, (◁))
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq ((:|>)))
import qualified Data.Sequence.Strict as Seq (fromList)
import Data.Set as Set
import Data.Word (Word64)
import GHC.Records (HasField (..))
import Zerepoch.V1.Ledger.Api (defaultCostModelParams)
import qualified ZerepochTx as P (Data (..))
import qualified ZerepochTx as Zerepoch
import Sophie.Spec.Ledger.PParams (Update)
import Sophie.Spec.Ledger.TxBody (DCert, TxIn, Wdrl)
import Sophie.Spec.Ledger.UTxO (UTxO (..), balance)
import Test.Bcc.Ledger.EvieEraGen (genValidityInterval)
import Test.Bcc.Ledger.Aurum.ZerepochScripts
  ( evenRedeemer2,
    evendata3,
    guessTheNumber3,
    oddRedeemer2,
    odddata3,
    redeemerIs102,
    sumsTo103,
  )
import Test.Bcc.Ledger.JenEraGen (addTokens, genMint, jenGenesisValue, policyIndex)
import Test.QuickCheck hiding ((><))
import Test.Sophie.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Sophie.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Sophie.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    ScriptInfo,
    TwoPhase2ArgInfo (..),
    TwoPhase3ArgInfo (..),
    findZerepoch,
    genNatural,
    hashData,
  )
import Test.Sophie.Spec.Ledger.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Sophie.Spec.Ledger.Generator.ScriptClass (Quantifier (..), ScriptClass (..))
import Test.Sophie.Spec.Ledger.Generator.Update (genM, genSophiePParamsDelta)
import qualified Test.Sophie.Spec.Ledger.Generator.Update as Sophie (genPParams)
import Test.Sophie.Spec.Ledger.Generator.Utxo (encodedLen, myDiscard)
import Test.Sophie.Spec.Ledger.SentryUtils (unsafeBoundRational)

-- ============================================================

isKeyHashAddr :: Addr crypto -> Bool
isKeyHashAddr (AddrBootstrap _) = True
isKeyHashAddr (Addr _ (KeyHashObj _) _) = True
isKeyHashAddr _ = False

-- | We are choosing new TxOut to pay fees, We want only Key locked addresss with Bcc only values.
vKeyLocked :: Mock c => Core.TxOut (AurumEra c) -> Bool
vKeyLocked txout =
  isKeyHashAddr (getField @"address" txout)
    && bccOnly (getField @"value" txout)

phase2scripts3Arg :: forall c. Mock c => [TwoPhase3ArgInfo (AurumEra c)]
phase2scripts3Arg =
  [ TwoPhase3ArgInfo (alwaysSucceeds 3) (hashScript @(AurumEra c) (alwaysSucceeds 3)) (P.I 1) (P.I 1, bigMem, bigStep) True,
    TwoPhase3ArgInfo guessTheNumber3 (hashScript @(AurumEra c) guessTheNumber3) (P.I 9) (P.I 9, bigMem, bigStep) True,
    TwoPhase3ArgInfo evendata3 (hashScript @(AurumEra c) evendata3) (P.I 8) (P.I 8, bigMem, bigStep) True,
    TwoPhase3ArgInfo odddata3 (hashScript @(AurumEra c) odddata3) (P.I 9) (P.I 9, bigMem, bigStep) True,
    TwoPhase3ArgInfo sumsTo103 (hashScript @(AurumEra c) sumsTo103) (P.I 1) (P.I 9, bigMem, bigStep) True,
    TwoPhase3ArgInfo (alwaysFails 3) (hashScript @(AurumEra c) (alwaysFails 3)) (P.I 1) (P.I 1, bigMem, bigStep) False
  ]

phase2scripts2Arg :: forall c. Mock c => [TwoPhase2ArgInfo (AurumEra c)]
phase2scripts2Arg =
  [ TwoPhase2ArgInfo (alwaysSucceeds 2) (hashScript @(AurumEra c) (alwaysSucceeds 2)) (P.I 1, bigMem, bigStep) True,
    TwoPhase2ArgInfo oddRedeemer2 (hashScript @(AurumEra c) oddRedeemer2) (P.I 13, bigMem, bigStep) True,
    TwoPhase2ArgInfo evenRedeemer2 (hashScript @(AurumEra c) evenRedeemer2) (P.I 14, bigMem, bigStep) True,
    TwoPhase2ArgInfo redeemerIs102 (hashScript @(AurumEra c) redeemerIs102) (P.I 10, bigMem, bigStep) True,
    TwoPhase2ArgInfo (alwaysFails 2) (hashScript @(AurumEra c) (alwaysFails 2)) (P.I 1, bigMem, bigStep) False
  ]

phase2scripts3ArgSucceeds :: forall c. Mock c => Script (AurumEra c) -> Bool
phase2scripts3ArgSucceeds script =
  case List.find (\info -> (getScript3 @(AurumEra c) info) == script) phase2scripts3Arg of
    Just i -> getSucceeds3 i
    Nothing -> True

phase2scripts2ArgSucceeds :: forall c. Mock c => Script (AurumEra c) -> Bool
phase2scripts2ArgSucceeds script =
  case List.find (\info -> (getScript2 @(AurumEra c) info) == script) phase2scripts2Arg of
    Just i -> getSucceeds2 i
    Nothing -> True

genZerepoch2Arg :: Mock c => Gen (Maybe (TwoPhase2ArgInfo (AurumEra c)))
genZerepoch2Arg = frequency [(10, Just <$> elements phase2scripts2Arg), (90, pure Nothing)]

-- | Gen a Mint value in the Aurum Era, with a 10% chance that it includes an AurumScript
genAurumMint :: Mock c => Value c -> Gen (Value c, [Aurum.Script (AurumEra c)])
genAurumMint startvalue = do
  ans <- genZerepoch2Arg
  case ans of
    Nothing -> pure (startvalue, [])
    Just (TwoPhase2ArgInfo script shash _ _) -> do
      count <- chooseEnum (1, 10)
      let assetname = AssetName . BS.pack $ "purple"
      pure (((valueFromList 0 [(PolicyID shash, assetname, count)]) <> startvalue), [script])

-- ================================================================

-- | A cost model that sets everything as being free
freeCostModel :: CostModel
freeCostModel = CostModel $ 0 <$ fromJust defaultCostModelParams

-- ================================================================

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = do a <- x; b <- y; pure (a, b)

genZerepochData :: Gen Zerepoch.Data
genZerepochData = resize 5 (sized gendata)
  where
    gendata n
      | n > 0 =
        oneof
          [ (Zerepoch.I <$> arbitrary),
            (Zerepoch.B <$> arbitrary),
            (Zerepoch.Map <$> listOf (genPair (gendata (n `div` 2)) (gendata (n `div` 2)))),
            (Zerepoch.Constr <$> arbitrary <*> listOf (gendata (n `div` 2))),
            (Zerepoch.List <$> listOf (gendata (n `div` 2)))
          ]
    gendata _ = oneof [Zerepoch.I <$> arbitrary, Zerepoch.B <$> arbitrary]

genSet :: Ord a => Gen a -> Gen (Set a)
genSet gen =
  frequency
    [ (1, pure Set.empty),
      (2, Set.fromList <$> sequence [gen]),
      (1, Set.fromList <$> sequence [gen, gen])
    ]

genAux :: forall c. Mock c => Constants -> Gen (StrictMaybe (Aurum.AuxiliaryData (AurumEra c)))
genAux constants =
  do
    maybeAux <- genEraAuxiliaryData @(JenEra c) constants
    case maybeAux of
      SNothing -> pure SNothing
      SJust (Jen.AuxiliaryData x y) ->
        SJust
          <$> ( Aurum.AuxiliaryData
                  <$> pure x
                  <*> pure (TimelockScript <$> y)
              )

instance CC.Crypto c => ScriptClass (AurumEra c) where
  -- basescript _ key = TimelockScript (basescript (Proxy @(JenEra c)) key) -- The old style from Jen
  basescript proxy key = (someLeaf proxy key)
  isKey _ (TimelockScript x) = isKey (Proxy @(JenEra c)) x
  isKey _ (ZerepochScript _) = Nothing
  isOnePhase _ (TimelockScript _) = True
  isOnePhase _ (ZerepochScript _) = False
  quantify _ (TimelockScript x) = fmap TimelockScript (quantify (Proxy @(JenEra c)) x)
  quantify _ x = Leaf x
  unQuantify _ quant = TimelockScript $ unQuantify (Proxy @(JenEra c)) (fmap unTime quant)

unTime :: Aurum.Script era -> Timelock (Crypto era)
unTime (TimelockScript x) = x
unTime (ZerepochScript _) = error "Zerepoch in Timelock"

okAsCollateral :: forall c. Mock c => UTxO (AurumEra c) -> TxIn c -> Bool
okAsCollateral utxo inputx =
  case Map.lookup inputx (unUTxO utxo) of
    Nothing -> False
    Just outputx -> vKeyLocked outputx

genAurumTxBody ::
  forall c.
  Mock c =>
  GenEnv (AurumEra c) ->
  UTxO (AurumEra c) ->
  Core.PParams (AurumEra c) ->
  SlotNo ->
  Set.Set (TxIn c) ->
  StrictSeq (TxOut (AurumEra c)) ->
  StrictSeq (DCert c) ->
  Wdrl c ->
  Coin ->
  StrictMaybe (Update (AurumEra c)) ->
  StrictMaybe (AuxiliaryDataHash c) ->
  Gen (TxBody (AurumEra c), [Core.Script (AurumEra c)])
genAurumTxBody _genenv utxo pparams currentslot input txOuts certs wdrls fee updates auxDHash = do
  _low <- genM (genSlotAfter currentslot)
  _high <- genM (genSlotAfter (currentslot + 50))
  netid <- genM $ pure Testnet -- frequency [(2, pure Mainnet), (1, pure Testnet)]
  startvalue <- genMint
  (minted, zerepochScripts) <- genAurumMint startvalue
  let (minted2, txouts2) = case addTokens (Proxy @(AurumEra c)) mempty pparams minted txOuts of
        Nothing -> (mempty, txOuts)
        Just os -> (minted, os)
      scriptsFromPolicies = List.map (\p -> (Map.!) policyIndex p) (Set.toList $ policies startvalue)
      txouts3 = fmap addMaybeDataHashToTxOut txouts2
  validityInterval <- genValidityInterval currentslot
  return
    ( TxBody
        input
        (Set.filter (okAsCollateral utxo) input) -- Set.empty -- collateral -- TODO do something better here (use genenv ?)
        txouts3
        certs
        wdrls
        fee
        validityInterval -- (ValidityInterval SNothing SNothing) -- (ValidityInterval low high)
        updates
        -- reqSignerHashes
        Set.empty -- TODO do something better here
        minted2
        -- scriptIntegrityHash starts out with empty Redeemers,
        -- as Remdeemers are added it is recomputed in updateEraTxBody
        (hashScriptIntegrity pparams (langsUsed @(AurumEra c) Map.empty) (Redeemers Map.empty) (TxDats Map.empty))
        auxDHash
        netid,
      (List.map TimelockScript scriptsFromPolicies <> zerepochScripts)
    )

genSlotAfter :: SlotNo -> Gen SlotNo
genSlotAfter currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

-- | Gen an Aurum PParamsDelta, by adding to a Sophie PParamsData
genAurumPParamsDelta ::
  forall c.
  Constants ->
  Aurum.PParams (AurumEra c) ->
  Gen (Core.PParamsDelta (AurumEra c))
genAurumPParamsDelta constants pp = do
  sophiepp <- genSophiePParamsDelta @(JenEra c) constants (Aurum.retractPP (Coin 100) pp)
  bcc <- genM (Coin <$> choose (1, 5))
  cost <- genM (pure (Map.singleton ZerepochV1 freeCostModel)) -- TODO what is a better assumption for this?
  let genPrice = unsafeBoundRational . (% 100) <$> choose (0, 200)
  price <- genM (Prices <$> genPrice <*> genPrice)
  mxTx <- pure SNothing -- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  -- Not too small for mxV, if this is too small then any Tx with Value
  -- that has lots of policyIds will fail. The Sophie Era uses hard coded 4000
  mxV <- genM (genNatural 4000 5000)
  let c = SJust 25 -- percent of fee in collateral
      mxC = SJust 100 -- max number of inputs in collateral
  pure (Aurum.extendPP sophiepp bcc cost price mxTx mxBl mxV c mxC)

genAurumPParams ::
  forall c.
  Constants ->
  Gen (Core.PParams (AurumEra c))
genAurumPParams constants = do
  sophiepp <- Sophie.genPParams @(JenEra c) constants -- This ensures that "_d" field is not 0.
  bcc <- (Coin <$> choose (1, 5))
  cost <- pure (Map.singleton ZerepochV1 freeCostModel) -- There are no other Languages, and there must be something for ZerepochV1
  price <- pure (Prices minBound minBound) -- (Prices <$> (Coin <$> choose (100, 5000)) <*> (Coin <$> choose (100, 5000)))
  mxTx <- pure (ExUnits (5 * bigMem + 1) (5 * bigStep + 1)) -- (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- (ExUnits <$> (choose ((20 * bigMem + 1), (30 * bigMem + 1))) <*> choose ((20 * bigStep + 1), (30 * bigStep + 1)))
  mxV <- (genNatural 4000 10000) -- This can't be too small. Sophie uses Hard coded 4000
  let c = 25 -- percent of fee in collateral
      mxC = 100 -- max number of inputs in collateral
  pure (Aurum.extendPP sophiepp bcc cost price mxTx mxBl mxV c mxC)

-- | Since Aurum PParams don't have this field, we have to compute something here.
instance HasField "_minUTxOValue" (Aurum.PParams (AurumEra c)) Coin where
  getField _ = Coin 4000

bigStep, bigMem :: Word64
bigStep = 99999 -- 999 -- 9999999990
bigMem = 50000 -- 500 -- 50000000

instance Mock c => EraGen (AurumEra c) where
  genEraAuxiliaryData = genAux
  genGenesisValue = jenGenesisValue
  genEraTwoPhase3Arg = phase2scripts3Arg
  genEraTwoPhase2Arg = phase2scripts2Arg

  genEraTxBody = genAurumTxBody
  updateEraTxBody utxo pp witnesses txb coinx txin txout = new
    where
      new =
        txb
          { inputs = (inputs txb) <> txin,
            collateral = (collateral txb) <> Set.filter (okAsCollateral utxo) txin, -- In Aurum, extra inputs also are added to collateral
            txfee = coinx,
            outputs = (outputs txb) :|> txout,
            -- The witnesses may have changed, recompute the scriptIntegrityHash.
            scriptIntegrityHash =
              hashScriptIntegrity
                pp
                (langsUsed @(AurumEra c) (getField @"txscripts" witnesses))
                (getField @"txrdmrs" witnesses)
                (getField @"txdats" witnesses)
          }

  addInputs txb txin = txb {inputs = (inputs txb) <> txin}

  genEraPParamsDelta = genAurumPParamsDelta
  genEraPParams = genAurumPParams
  genEraWitnesses (utxo, txbody, scriptinfo) setWitVKey mapScriptWit = new
    where
      new =
        TxWitness
          setWitVKey
          Set.empty
          mapScriptWit
          -- (dataMapFromTxOut (Prelude.foldr (:) [] (outputs' txbody)) (TxDats (getDataMap scriptinfo mapScriptWit)))
          (dataMapFromTxOut (Prelude.foldr (:) [] (Map.elems smallUtxo)) (TxDats (getDataMap scriptinfo mapScriptWit)))
          -- The data hashes come from two places
          (Redeemers rdmrMap)
      txinputs = inputs' txbody
      smallUtxo = eval (txinputs ◁ utxo) :: Map.Map (TxIn c) (Core.TxOut (AurumEra c))
      purposeHashPairs = scriptsNeededFromBody @(AurumEra c) utxo txbody
      rdmrMap = List.foldl' accum Map.empty purposeHashPairs -- Search through the pairs for Zerepoch scripts
      accum ans (purpose, hash1) =
        case Map.lookup hash1 mapScriptWit of
          Nothing -> ans
          Just script ->
            if isNativeScript @(AurumEra c) script
              then ans -- Native scripts don't have redeemers
              else case Map.lookup hash1 (fst scriptinfo) of -- It could be one of the known 3-Arg Zerepoch Scripts
                Just info -> addRedeemMap txbody (getRedeemer3 info) purpose ans -- Add it to the redeemer map
                Nothing -> case Map.lookup hash1 (snd scriptinfo) of -- It could be one of the known 2-Arg Zerepoch Scripts
                  Just info -> addRedeemMap txbody (getRedeemer2 info) purpose ans -- Add it to the redeemer map
                  Nothing -> ans

  constructTx bod wit auxdata = ValidatedTx bod wit (IsValid v) auxdata
    where
      v = all twoPhaseValidates (txscripts' wit)
      twoPhaseValidates script =
        (isNativeScript @(AurumEra c) script)
          || (phase2scripts3ArgSucceeds script && phase2scripts2ArgSucceeds script)

  genEraGoodTxOut = vKeyLocked

  genEraScriptCost pp script =
    if isZerepochScript script
      then case List.find (\info -> (getScript3 @(AurumEra c) info) == script) genEraTwoPhase3Arg of
        Just (TwoPhase3ArgInfo _script _hash inputdata (rdmr, mems, steps) _succeed) ->
          txscriptfee (getField @"_prices" pp) (ExUnits mems steps)
            <+> storageCost 10 pp (rdmr, ExUnits mems steps) -- Extra 10 for the RdmrPtr
            <+> storageCost 32 pp inputdata -- Extra 32 for the hash
            <+> storageCost 0 pp script
        Nothing -> storageCost 0 pp script
      else storageCost 0 pp script

  genEraDone pp tx =
    let txb = getField @"body" tx
        theFee = getField @"txfee" txb -- Coin supplied to pay fees
        minimumFee = minfee @(AurumEra c) pp tx
     in if (minimumFee <= theFee)
          then (pure tx)
          else myDiscard "MinFeee violation: genEraDne: AurumEraGem.hs"

  genEraTweakBlock pp txns =
    let txTotal, ppMax :: ExUnits
        txTotal = Prelude.foldr (<>) mempty (fmap totExUnits txns)
        ppMax = getField @"_maxBlockExUnits" pp
     in if pointWiseExUnits (<=) txTotal ppMax
          then pure txns
          else myDiscard "TotExUnits violation: genEraTweakBlock: AurumEraGem.hs"

  hasFailedScripts = (== IsValid False) . (getField @"isValid")

  feeOrCollateral tx utxo =
    case getField @"isValid" tx of
      IsValid True -> getField @"txfee" $ getField @"body" tx
      IsValid False -> sumCollateral tx utxo

sumCollateral ::
  forall era.
  ( Era era,
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  Core.Tx era ->
  UTxO era ->
  Coin
sumCollateral tx (UTxO utxo) =
  coin . balance @era . UTxO . eval $ collateral_ ◁ utxo
  where
    collateral_ = getField @"collateral" . getField @"body" $ tx

storageCost :: ToCBOR t => Integer -> (Aurum.PParams era) -> t -> Coin
storageCost extra pp x = (extra + encodedLen x) <×> Coin (fromIntegral (getField @"_minfeeA" pp))

addRedeemMap ::
  forall c.
  TxBody (AurumEra c) ->
  (Zerepoch.Data, Word64, Word64) ->
  ScriptPurpose c ->
  Map RdmrPtr (Data (AurumEra c), ExUnits) ->
  Map RdmrPtr (Data (AurumEra c), ExUnits)
addRedeemMap body1 (dat, space, steps) purpose ans =
  case (purpose, rdptr @(AurumEra c) body1 purpose) of
    (Spending _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Minting _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Rewarding _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Certifying _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    _ -> ans

getDataMap :: forall era. Era era => ScriptInfo era -> Map (ScriptHash (Crypto era)) (Core.Script era) -> Map (DataHash (Crypto era)) (Data era)
getDataMap (scriptinfo3, _) scrips = Map.foldlWithKey' accum Map.empty scrips
  where
    accum ans hsh _script =
      case Map.lookup hsh scriptinfo3 of
        Nothing -> ans
        Just (TwoPhase3ArgInfo _script _hash dat _redeem _) ->
          Map.insert (hashData @era dat) (Data dat) ans

instance Mock c => MinGenTxout (AurumEra c) where
  calcEraMinUTxO tout pp = (utxoEntrySize tout <×> getField @"_coinsPerUTxOWord" pp)
  addValToTxOut v (TxOut a u _b) = TxOut a (v <+> u) (dataFromAddr a) -- _b
  genEraTxOut genv genVal addrs = do
    values <- (replicateM (length addrs) genVal)
    let makeTxOut (addr@(Addr _network (ScriptHashObj shash) _stakeref)) val = TxOut addr val maybedatahash
          where
            (_, maybedatahash) = findZerepoch genv shash
        makeTxOut addr val = TxOut addr val SNothing
    pure (zipWith makeTxOut addrs values)

-- | If an Address is script address, we can find a potential data hash for it from
--   genEraTwoPhase3Arg, which contains all known 3 arg zerepoch scripts in the tests set.
-- If the script has is not in that map, then its data hash is SNothing.
dataFromAddr :: forall c. Mock c => Addr c -> StrictMaybe (DataHash c)
dataFromAddr (Addr _network (ScriptHashObj shash) _stakeref) =
  case List.find (\info -> shash == hashScript @(AurumEra c) (getScript3 @(AurumEra c) info)) genEraTwoPhase3Arg of
    Just info -> SJust (hashData @(AurumEra c) (getData3 info))
    Nothing -> SNothing
dataFromAddr _ = SNothing

-- | We can find the data associated with the data hashes in the TxOuts, since
--   genEraTwoPhase3Arg, which contains all known 3 arg zerepoch scripts stores the data.
dataMapFromTxOut :: forall c. Mock c => [TxOut (AurumEra c)] -> TxDats (AurumEra c) -> TxDats (AurumEra c)
dataMapFromTxOut txouts datahashmap = Prelude.foldl accum datahashmap txouts
  where
    accum !ans (TxOut _ _ SNothing) = ans
    accum !ans (TxOut _ _ (SJust dhash)) =
      case List.find (\info -> hashData @(AurumEra c) (getData3 info) == dhash) (genEraTwoPhase3Arg @(AurumEra c)) of
        Just info -> let TxDats' m = ans in TxDats (Map.insert dhash (Data (getData3 info)) m)
        Nothing -> ans

addMaybeDataHashToTxOut :: Mock c => TxOut (AurumEra c) -> TxOut (AurumEra c)
addMaybeDataHashToTxOut (TxOut addr val _) = TxOut addr val (dataFromAddr addr)

someLeaf ::
  forall era.
  Era era =>
  Proxy era ->
  KeyHash 'Witness (Crypto era) ->
  Script era
someLeaf _proxy x =
  let n = hash (serializeEncoding' (toCBOR x)) -- We don't really care about the hash, we only
      slot = SlotNo (fromIntegral (mod n 200)) -- use it to pseudo-randomly pick a slot and mode
      mode = mod n 3 -- mode==0 is a time leaf,  mode 1 or 2 is a signature leaf
   in case mode of
        0 -> TimelockScript $ (RequireAnyOf . Seq.fromList) [RequireTimeStart slot, RequireTimeExpire slot]
        _ -> TimelockScript $ RequireSignature x
