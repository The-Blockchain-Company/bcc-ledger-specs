{-# LANGUAGE DuplicateRecordFields #-}

module Sophie.Spec.Ledger.API.Types
  ( module X,
  )
where

import Bcc.Ledger.Address as X
  ( Addr (..),
    RewardAcnt (..),
  )
import Bcc.Ledger.BaseTypes as X
  ( Globals (..),
    Network (..),
    Nonce (..),
    Port (..),
    StrictMaybe (..),
    epochInfo,
  )
import Bcc.Ledger.Coin as X
  ( Coin (..),
    word64ToCoin,
  )
import Bcc.Ledger.Credential as X
  ( Credential (..),
    StakeReference (..),
    VestedReference (..),
    VestedCredential (..),
  )
import Bcc.Ledger.Keys as X
  ( CertifiedVRF,
    GenDelegPair (..),
    GenDelegs (..),
    VestedDelegPair (..),
    VestedDelegs (..),
    Hash,
    KESignable,
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    SignKeyDSIGN,
    SignKeyKES,
    SignKeyVRF,
    SignedDSIGN,
    SignedKES,
    VKey (..),
    VerKeyKES,
    VerKeyVRF,
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
  )
import Bcc.Protocol.TOptimum as X -- #TODO Sophie version depracated, pull in Sentry Type from Cole
  ( PoolDistr (..),
    individualPoolStake,
  )
import Bcc.Protocol.TOptimum.BHeader as X
  ( BHBody (..),
    BHeader (..),
    HashHeader (..),
    PrevHash (..),
    bHeaderSize,
    bhHash,
    bhbody,
  )
import Bcc.Protocol.TOptimum.OCert as X (KESPeriod (..), OCert (..))
import Bcc.Protocol.TOptimum.Rules.OCert as X (OCertEnv (..))
import Bcc.Protocol.TOptimum.Rules.Overlay as X
  ( OBftSlot (..),
    classifyOverlaySlot,
    isOverlaySlot,
    lookupInOverlaySchedule,
  )
import Bcc.Protocol.TOptimum.Rules.Prtcl as X
  ( PrtclEnv (..),
    PrtclPredicateFailure (..),
    PrtclState (..),
    PrtlSeqFailure (..),
    prtlSeqChecks,
  )
import Sophie.Spec.Ledger.Address.Bootstrap as X
  ( BootstrapWitness (..),
  )
import Sophie.Spec.Ledger.BlockChain as X
  ( Block (..),
    LaxBlock (..),
    bbHash,
    bbody,
    bheader,
  )
import Sophie.Spec.Ledger.Delegation.Certificates as X
  ( DCert (..),
    DelegCert (..),
    VestedDelegCert (..),
    PoolCert (..),
  )
import Sophie.Spec.Ledger.EpochBoundary as X
  ( SnapShot (..),
    SnapShots (..),
    Stake (..),
  )
import Sophie.Spec.Ledger.Genesis as X
import Sophie.Spec.Ledger.LedgerState as X
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    InstantaneousRewards (..),
    KeyPairs,
    LedgerState (..),
    NewEpochState (..),
    PPUPState (..),
    PState (..),
    RewardUpdate (..),
    UTxOState (..),
    WitHashes (..),
  )
import Sophie.Spec.Ledger.Metadata as X
  ( Metadata (..),
    Metadatum (..),
  )
import Sophie.Spec.Ledger.PParams as X
  ( PParams,
    PParams' (..),
    ProposedPPUpdates (..),
    ProtVer (..),
    Update (..),
  )
import Sophie.Spec.Ledger.Rewards as X
  ( NonMyopic,
  )
import Sophie.Spec.Ledger.STS.Chain as X
  ( CHAIN,
    ChainState (..),
    initialSophieState,
  )
import Sophie.Spec.Ledger.STS.Deleg as X (DELEG, DelegEnv (..))
import Sophie.Spec.Ledger.STS.Delegs as X (DELEGS, DelegsEnv (..))
import Sophie.Spec.Ledger.STS.Delpl as X (DELPL, DelplEnv (..))
import Sophie.Spec.Ledger.STS.Ledger as X (LEDGER, LedgerEnv (..))
import Sophie.Spec.Ledger.STS.Ledgers as X (LEDGERS, LedgersEnv (..))
import Sophie.Spec.Ledger.STS.NewEpoch as X
  ( NEWEPOCH,
    calculatePoolDistr,
  )
import Sophie.Spec.Ledger.STS.Pool as X (POOL, PoolEnv (..))
import Sophie.Spec.Ledger.STS.PoolReap as X (POOLREAP)
import Sophie.Spec.Ledger.STS.Ppup as X (PPUP, PPUPEnv (..))
import Sophie.Spec.Ledger.STS.Tick as X (TICK)
import Sophie.Spec.Ledger.STS.Tickn as X
  ( TICKN,
    TicknEnv (..),
    TicknPredicateFailure,
    TicknState (..),
  )
import Sophie.Spec.Ledger.STS.Utxo as X
  ( UTXO,
    UtxoEnv (..),
  )
import Sophie.Spec.Ledger.STS.Utxow as X (UTXOW)
import Sophie.Spec.Ledger.Scripts as X
  ( MultiSig (..),
    ScriptHash (..),
  )
import Sophie.Spec.Ledger.StabilityWindow as X
  ( computeRandomnessStabilisationWindow,
    computeStabilityWindow,
  )
import Sophie.Spec.Ledger.Tx as X
  ( Tx (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    WitnessSet,
  )
import Sophie.Spec.Ledger.TxBody as X
  ( Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolMetadata (..),
    PoolParams (..),
    Ptr (..),
    StakeCreds (..),
    StakePoolRelay (..),
    TxId (..),
    Wdrl (..),
    WitVKey (..),
  )
import Sophie.Spec.Ledger.UTxO as X
  ( UTxO (..),
    balance,
  )
