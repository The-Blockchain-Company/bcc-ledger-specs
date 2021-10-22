{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sophie.Spec.Ledger.Examples.Consensus where

import Bcc.Binary
import Bcc.Crypto.DSIGN as DSIGN
import Bcc.Crypto.Hash as Hash
import Bcc.Crypto.Seed as Seed
import Bcc.Crypto.VRF as VRF
import Bcc.Ledger.AuxiliaryData
import Bcc.Ledger.BaseTypes
import Bcc.Ledger.Coin
import qualified Bcc.Ledger.Core as Core
import Bcc.Ledger.Crypto
import Bcc.Ledger.Era
import Bcc.Ledger.Keys
import Bcc.Ledger.SafeHash
import Bcc.Ledger.Serialization
import Bcc.Ledger.Sophie (SophieEra)
import Bcc.Ledger.Sophie.Constraints
import Bcc.Protocol.TOptimum
import Bcc.Slotting.Block
import Bcc.Slotting.EpochInfo
import Bcc.Slotting.Slot
import Control.State.Transition.Extended
import qualified Data.ByteString as Strict
import Data.Coerce (coerce)
import Data.Default.Class
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Word (Word64, Word8)
import GHC.Records (HasField)
import Numeric.Natural (Natural)
import Sophie.Spec.Ledger.API
import Sophie.Spec.Ledger.EpochBoundary
import Sophie.Spec.Ledger.LedgerState
import Sophie.Spec.Ledger.PParams
import Sophie.Spec.Ledger.STS.Delegs
import Sophie.Spec.Ledger.STS.Ledger
import Sophie.Spec.Ledger.Tx
import Sophie.Spec.Ledger.UTxO
import Test.Sophie.Spec.Ledger.Generator.Core
import Test.Sophie.Spec.Ledger.Orphans ()
import Test.Sophie.Spec.Ledger.SentryUtils hiding (mkVRFKeyPair)

type KeyPairWits era = [KeyPair 'Witness (Bcc.Ledger.Era.Crypto era)]

{-------------------------------------------------------------------------------
  SophieLedgerExamples
-------------------------------------------------------------------------------}

data SophieResultExamples era = SophieResultExamples
  { srePParams :: Core.PParams era,
    sreProposedPPUpdates :: ProposedPPUpdates era,
    srePoolDistr :: PoolDistr (Bcc.Ledger.Era.Crypto era),
    sreNonMyopicRewards ::
      Map
        (Either Coin (Credential 'Staking (Bcc.Ledger.Era.Crypto era)))
        (Map (KeyHash 'StakePool (Bcc.Ledger.Era.Crypto era)) Coin),
    sreSophieGenesis :: SophieGenesis era
  }

data SophieLedgerExamples era = SophieLedgerExamples
  { sleBlock :: Block era,
    sleHashHeader :: HashHeader (Bcc.Ledger.Era.Crypto era),
    sleTx :: Core.Tx era,
    sleApplyTxError :: ApplyTxError era,
    sleRewardsCredentials :: Set (Either Coin (Credential 'Staking (Bcc.Ledger.Era.Crypto era))),
    sleResultExamples :: SophieResultExamples era,
    sleNewEpochState :: NewEpochState era,
    sleChainDepState :: ChainDepState (Bcc.Ledger.Era.Crypto era)
  }

{-------------------------------------------------------------------------------
  Default constructor
-------------------------------------------------------------------------------}

type SophieBasedEra' era =
  ( SophieBasedEra era,
    ToCBORGroup (TxSeq era),
    ToCBOR (Core.Witnesses era),
    Default (State (Core.EraRule "PPUP" era))
  )

defaultSophieLedgerExamples ::
  forall era.
  ( SophieBasedEra' era,
    PredicateFailure (Core.EraRule "DELEGS" era)
      ~ DelegsPredicateFailure era,
    Core.PParams era ~ Sophie.Spec.Ledger.PParams.PParams era,
    Core.PParamsDelta era ~ PParams' StrictMaybe era
  ) =>
  (Core.TxBody era -> KeyPairWits era -> Core.Witnesses era) ->
  (Tx era -> Core.Tx era) ->
  Core.Value era ->
  Core.TxBody era ->
  Core.AuxiliaryData era ->
  SophieLedgerExamples era
defaultSophieLedgerExamples mkWitnesses mkValidatedTx value txBody auxData =
  SophieLedgerExamples
    { sleBlock = exampleSophieLedgerBlock (mkValidatedTx tx),
      sleHashHeader = exampleHashHeader (Proxy @era),
      sleTx = mkValidatedTx tx,
      sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @era (mkKeyHash 1),
      sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100),
            Right (ScriptHashObj (mkScriptHash 1)),
            Right (KeyHashObj (mkKeyHash 2))
          ],
      sleResultExamples = resultExamples,
      sleNewEpochState =
        exampleNewEpochState
          value
          emptyPParams
          (emptyPParams {_minUTxOValue = Coin 1}),
      sleChainDepState = exampleLedgerChainDepState 1
    }
  where
    tx = exampleTx mkWitnesses txBody auxData

    resultExamples =
      SophieResultExamples
        { srePParams = def,
          sreProposedPPUpdates = exampleProposedPParamsUpdates,
          srePoolDistr = examplePoolDistr,
          sreNonMyopicRewards = exampleNonMyopicRewards,
          sreSophieGenesis = testSophieGenesis
        }

{-------------------------------------------------------------------------------
  Helper constructors
-------------------------------------------------------------------------------}

exampleSophieLedgerBlock ::
  forall era.
  SophieBasedEra' era =>
  Core.Tx era ->
  Block era
exampleSophieLedgerBlock tx = Block blockHeader blockBody
  where
    keys :: AllIssuerKeys (Bcc.Ledger.Era.Crypto era) 'StakePool
    keys = exampleKeys

    (_, (hotKey, _)) = head $ hot keys
    KeyPair vKeyCold _ = cold keys

    blockHeader :: BHeader (Bcc.Ledger.Era.Crypto era)
    blockHeader = BHeader blockHeaderBody (signedKES () 0 blockHeaderBody hotKey)

    blockHeaderBody :: BHBody (Bcc.Ledger.Era.Crypto era)
    blockHeaderBody =
      BHBody
        { bheaderBlockNo = BlockNo 3,
          bheaderSlotNo = SlotNo 9,
          bheaderPrev = BlockHash (HashHeader (mkDummyHash Proxy 2)),
          bheaderVk = coerceKeyRole vKeyCold,
          bheaderVrfVk = snd $ vrf keys,
          bheaderEta = mkCertifiedVRF (mkBytes 0) (fst $ vrf keys),
          bheaderL = mkCertifiedVRF (mkBytes 1) (fst $ vrf keys),
          bsize = 2345,
          bhash = hashTxSeq @era blockBody,
          bheaderOCert = mkOCert keys 0 (KESPeriod 0),
          bprotver = ProtVer 2 0
        }

    blockBody = toTxSeq @era (StrictSeq.fromList [tx])

    mkBytes :: Int -> Bcc.Ledger.BaseTypes.Seed
    mkBytes = Seed . mkDummyHash (Proxy @Blake2b_256)

exampleHashHeader ::
  forall era.
  SophieBasedEra' era =>
  Proxy era ->
  HashHeader (Bcc.Ledger.Era.Crypto era)
exampleHashHeader _ = coerce $ mkDummyHash (Proxy @(HASH (Bcc.Ledger.Era.Crypto era))) 0

mkKeyHash :: forall c discriminator. Bcc.Ledger.Crypto.Crypto c => Int -> KeyHash discriminator c
mkKeyHash = KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

mkScriptHash :: forall c. Bcc.Ledger.Crypto.Crypto c => Int -> ScriptHash c
mkScriptHash = ScriptHash . mkDummyHash (Proxy @(ADDRHASH c))

-- | This is not a valid transaction. We don't care, we are only interested in
-- serialisation, not validation.
exampleTx ::
  forall era.
  SophieBasedEra' era =>
  (Core.TxBody era -> KeyPairWits era -> Core.Witnesses era) ->
  Core.TxBody era ->
  Core.AuxiliaryData era ->
  Tx era
exampleTx mkWitnesses txBody auxData =
  Tx txBody (mkWitnesses txBody keyPairWits) (SJust auxData)
  where
    keyPairWits :: KeyPairWits era
    keyPairWits =
      [ asWitness examplePayKey,
        asWitness exampleStakeKey,
        asWitness $ cold (exampleKeys @(Bcc.Ledger.Era.Crypto era) @'StakePool)
      ]

exampleProposedPParamsUpdates ::
  ( SophieBasedEra' era,
    Core.PParamsDelta era ~ PParams' StrictMaybe era
  ) =>
  ProposedPPUpdates era
exampleProposedPParamsUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 0)
      (emptyPParamsUpdate {_keyDeposit = SJust (Coin 100)})

examplePoolDistr :: forall c. OptimumCrypto c => PoolDistr c
examplePoolDistr =
  PoolDistr $
    Map.fromList
      [ ( mkKeyHash 1,
          IndividualPoolStake
            1
            (hashVerKeyVRF (snd (vrf (exampleKeys @c))))
        )
      ]

exampleNonMyopicRewards ::
  forall c.
  OptimumCrypto c =>
  Map
    (Either Coin (Credential 'Staking c))
    (Map (KeyHash 'StakePool c) Coin)
exampleNonMyopicRewards =
  Map.fromList
    [ (Left (Coin 100), Map.singleton (mkKeyHash 2) (Coin 3)),
      (Right (ScriptHashObj (mkScriptHash 1)), Map.empty),
      (Right (KeyHashObj (mkKeyHash 2)), Map.singleton (mkKeyHash 3) (Coin 9))
    ]

-- | These are dummy values.
testSophieGenesis :: SophieGenesis era
testSophieGenesis =
  SophieGenesis
    { sgSystemStart = UTCTime (fromGregorian 2020 5 14) 0,
      sgNetworkMagic = 0,
      sgNetworkId = Testnet,
      -- Chosen to match activeSlotCoeff
      sgActiveSlotsCoeff = unsafeBoundRational 0.9,
      sgSecurityParam = securityParameter testGlobals,
      sgVestMultiple = vestMultiple testGlobals,
      sgEpochLength = runIdentity $ epochInfoSize testEpochInfo 0,
      sgSlotsPerKESPeriod = slotsPerKESPeriod testGlobals,
      sgMaxKESEvolutions = maxKESEvo testGlobals,
      -- Not important
      sgSlotLength = secondsToNominalDiffTime 2,
      sgUpdateQuorum = quorum testGlobals,
      sgMaxEntropicSupply = maxEntropicSupply testGlobals,
      sgProtocolParams = emptyPParams,
      sgGenDelegs = Map.empty,
      sgVestedDelegs = Map.empty,
      sgInitialFunds = Map.empty,
      sgStaking = emptyGenesisStaking
    }

-- | This is probably not a valid ledger. We don't care, we are only
-- interested in serialisation, not validation.
exampleNewEpochState ::
  forall era.
  ( SophieBasedEra' era,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval
  ) =>
  Core.Value era ->
  Core.PParams era ->
  Core.PParams era ->
  NewEpochState era
exampleNewEpochState value ppp pp =
  NewEpochState
    { nesEL = EpochNo 0,
      nesBprev = BlocksMade (Map.singleton (mkKeyHash 1) 10),
      nesBcur = BlocksMade (Map.singleton (mkKeyHash 2) 3),
      nesEs = epochState,
      nesRu = SJust rewardUpdate,
      nesPd = examplePoolDistr
    }
  where
    epochState :: EpochState era
    epochState =
      EpochState
        { esAccountState =
            AccountState
              { _treasury = Coin 10000,
                _reserves = Coin 1000
              },
          esSnapshots = emptySnapShots,
          esLState =
            LedgerState
              { _utxoState =
                  UTxOState
                    { _utxo =
                        UTxO $
                          Map.fromList
                            [ ( TxIn (TxId (mkDummySafeHash Proxy 1)) 0,
                                makeTxOut (Proxy @era) addr value
                              )
                            ],
                      _deposited = Coin 1000,
                      _fees = Coin 1,
                      _ppups = def
                    },
                _delegationState = def
              },
          esPrevPp = ppp,
          esPp = pp,
          esNonMyopic = def
        }
      where
        addr :: Addr (Bcc.Ledger.Era.Crypto era)
        addr =
          Addr
            Testnet
            (keyToCredential examplePayKey)
            (StakeRefBase (keyToCredential exampleStakeKey))

    rewardUpdate :: PulsingRewUpdate (Bcc.Ledger.Era.Crypto era)
    (rewardUpdate, _) =
      startStep @era
        (EpochSize 432000)
        (BlocksMade (Map.singleton (mkKeyHash 1) 10))
        epochState
        (Coin 45)
        (activeSlotCoeff testGlobals)
        10

exampleLedgerChainDepState :: forall c. OptimumCrypto c => Word64 -> ChainDepState c
exampleLedgerChainDepState seed =
  ChainDepState
    { csProtocol =
        PrtclState
          ( Map.fromList
              [ (mkKeyHash 1, 1),
                (mkKeyHash 2, 2)
              ]
          )
          (mkNonceFromNumber seed)
          (mkNonceFromNumber seed),
      csTickn =
        TicknState
          NeutralNonce
          (mkNonceFromNumber seed),
      csLabNonce =
        mkNonceFromNumber seed
    }

testEpochInfo :: EpochInfo Identity
testEpochInfo = epochInfo testGlobals

mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
mkDummyHash _ = coerce . hashWithSerialiser @h toCBOR

mkDummySafeHash :: forall c a. Bcc.Ledger.Crypto.Crypto c => Proxy c -> Int -> SafeHash c a
mkDummySafeHash _ =
  unsafeMakeSafeHash
    . mkDummyHash (Proxy @(HASH c))

{-------------------------------------------------------------------------------
  Sophie era specific functions
-------------------------------------------------------------------------------}

type StandardSophie = SophieEra StandardCrypto

-- | SophieLedgerExamples for Sophie era
ledgerExamplesSophie :: SophieLedgerExamples StandardSophie
ledgerExamplesSophie =
  defaultSophieLedgerExamples
    (mkWitnessesPreAurum (Proxy @StandardSophie))
    id
    exampleCoin
    exampleTxBodySophie
    exampleAuxiliaryDataSophie

mkWitnessesPreAurum ::
  SophieBasedEra' era =>
  Proxy era ->
  Core.TxBody era ->
  KeyPairWits era ->
  WitnessSet era
mkWitnessesPreAurum _ txBody keyPairWits =
  mempty
    { addrWits =
        makeWitnessesVKey (coerce (hashAnnotated txBody)) keyPairWits
    }

exampleCoin :: Coin
exampleCoin = Coin 10

exampleTxBodySophie :: Sophie.Spec.Ledger.API.TxBody StandardSophie
exampleTxBodySophie =
  Sophie.Spec.Ledger.API.TxBody
    exampleTxIns
    ( StrictSeq.fromList
        [ TxOut (mkAddr (examplePayKey, exampleStakeKey)) (Coin 100000)
        ]
    )
    exampleCerts
    exampleWithdrawals
    (Coin 3)
    (SlotNo 10)
    (SJust (Update exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
  where
    -- Dummy hash to decouple from the auxiliaryData in 'exampleTx'.
    auxiliaryDataHash :: AuxiliaryDataHash StandardCrypto
    auxiliaryDataHash =
      AuxiliaryDataHash $ mkDummySafeHash (Proxy @StandardCrypto) 30

exampleMetadataMap :: Map Word64 Metadatum
exampleMetadataMap =
  Map.fromList
    [ (1, S "string"),
      (2, B "bytes"),
      (3, List [I 1, I 2]),
      (4, Map [(I 3, B "b")])
    ]

exampleAuxiliaryDataSophie :: Core.AuxiliaryData StandardSophie
exampleAuxiliaryDataSophie = Metadata exampleMetadataMap

exampleTxIns :: Bcc.Ledger.Crypto.Crypto c => Set (TxIn c)
exampleTxIns =
  Set.fromList
    [ TxIn (TxId (mkDummySafeHash Proxy 1)) 0
    ]

exampleCerts :: Bcc.Ledger.Crypto.Crypto c => StrictSeq (DCert c)
exampleCerts =
  StrictSeq.fromList
    [ DCertDeleg (RegKey (keyToCredential exampleStakeKey)),
      DCertPool (RegPool examplePoolParams),
      DCertMir $
        MIRCert ReservesMIR $
          StakeAddressesMIR $
            Map.fromList
              [ (keyToCredential (mkDSIGNKeyPair 2), DeltaCoin 110)
              ]
    ]

exampleWithdrawals :: Bcc.Ledger.Crypto.Crypto c => Wdrl c
exampleWithdrawals =
  Wdrl $
    Map.fromList
      [ (_poolRAcnt examplePoolParams, Coin 100)
      ]

exampleProposedPPUpdates ::
  ( Core.PParamsDelta era ~ PParams' StrictMaybe era,
    SophieBasedEra' era
  ) =>
  ProposedPPUpdates era
exampleProposedPPUpdates =
  ProposedPPUpdates $
    Map.singleton
      (mkKeyHash 1)
      (emptyPParamsUpdate {_maxBHSize = SJust 4000})

examplePayKey :: Bcc.Ledger.Crypto.Crypto c => KeyPair 'Payment c
examplePayKey = mkDSIGNKeyPair 0

exampleStakeKey :: Bcc.Ledger.Crypto.Crypto c => KeyPair 'Staking c
exampleStakeKey = mkDSIGNKeyPair 1

exampleKeys :: forall c r. Bcc.Ledger.Crypto.Crypto c => AllIssuerKeys c r
exampleKeys =
  AllIssuerKeys
    coldKey
    (mkVRFKeyPair (Proxy @c) 1)
    [(KESPeriod 0, mkKESKeyPair (RawSeed 1 0 0 0 3))]
    (hashKey (vKey coldKey))
  where
    coldKey = mkDSIGNKeyPair 1

keyToCredential :: Bcc.Ledger.Crypto.Crypto c => KeyPair r c -> Credential r c
keyToCredential = KeyHashObj . hashKey . vKey

-- | @mkKeyPair'@ from @Test.Sophie.Spec.Ledger.SentryUtils@ doesn't work for real
-- crypto:
-- <https://github.com/The-Blockchain-Company/bcc-ledger-specs/issues/1770>
mkDSIGNKeyPair ::
  forall c kd.
  DSIGNAlgorithm (DSIGN c) =>
  Word8 ->
  KeyPair kd c
mkDSIGNKeyPair byte = KeyPair (VKey $ DSIGN.deriveVerKeyDSIGN sk) sk
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (DSIGN.seedSizeDSIGN (Proxy @(DSIGN c))))
          byte

    sk = DSIGN.genKeyDSIGN seed

mkVRFKeyPair ::
  forall c.
  VRFAlgorithm (VRF c) =>
  Proxy c ->
  Word8 ->
  (Bcc.Ledger.Keys.SignKeyVRF c, Bcc.Ledger.Keys.VerKeyVRF c)
mkVRFKeyPair _ byte = (sk, VRF.deriveVerKeyVRF sk)
  where
    seed =
      Seed.mkSeedFromBytes $
        Strict.replicate
          (fromIntegral (VRF.seedSizeVRF (Proxy @(VRF c))))
          byte

    sk = VRF.genKeyVRF seed

examplePoolParams :: forall c. Bcc.Ledger.Crypto.Crypto c => PoolParams c
examplePoolParams =
  PoolParams
    { _poolId = hashKey $ vKey $ cold poolKeys,
      _poolVrf = hashVerKeyVRF $ snd $ vrf poolKeys,
      _poolPledge = Coin 1,
      _poolCost = Coin 5,
      _poolMargin = unsafeBoundRational 0.1,
      _poolRAcnt = RewardAcnt Testnet (keyToCredential exampleStakeKey),
      _poolOwners = Set.singleton $ hashKey $ vKey exampleStakeKey,
      _poolRelays = StrictSeq.empty,
      _poolMD =
        SJust $
          PoolMetadata
            { _poolMDUrl = fromJust $ textToUrl "consensus.pool",
              _poolMDHash = "{}"
            }
    }
  where
    poolKeys = exampleKeys @c @'StakePool
