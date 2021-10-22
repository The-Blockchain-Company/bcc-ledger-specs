{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Sophie.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    DelegCert (..),
    PoolCert (..),
    GenesisDelegCert (..),
    VestedDelegCert (..),
    MIRCert (..),
    StakeCreds (..),
    delegCWitness,
    poolCWitness,
    genesisCWitness,
    vestedCWitness,
    isRegKey,
    isDeRegKey,
    isDelegation,
    isGenesisDelegation,
    isVestedDelegation, 
    isRegPool,
    isRetirePool,
    isInstantaneousRewards,
    isReservesMIRCert,
    isTreasuryMIRCert,
    requiresVKeyWitness,
    --Deprecated
    PoolDistr,
    IndividualPoolStake,
  )
where

import Bcc.Ledger.Credential (Credential (..))
import Bcc.Ledger.Keys (KeyHash, KeyRole (..))
import qualified Bcc.Protocol.TOptimum as TP
import Sophie.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    VestedDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    PoolCert (..),
    PoolParams (..),
    StakeCreds (..),
  )

-- | Determine the certificate author
delegCWitness :: DelegCert crypto -> Credential 'Staking crypto
delegCWitness (RegKey _) = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk) = hk
delegCWitness (Delegate delegation) = _delegator delegation

poolCWitness :: PoolCert crypto -> Credential 'StakePool crypto
poolCWitness (RegPool pool) = KeyHashObj $ _poolId pool
poolCWitness (RetirePool k _) = KeyHashObj k

genesisCWitness :: GenesisDelegCert crypto -> KeyHash 'Genesis crypto
genesisCWitness (GenesisDelegCert gk _ _) = gk

vestedCWitness :: VestedDelegCert crypto -> KeyHash 'Vested crypto 
vestedCWitness (VestedDelegCert ak _ _) = ak

-- | Check for 'RegKey' constructor
isRegKey :: DCert crypto -> Bool
isRegKey (DCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for 'DeRegKey' constructor
isDeRegKey :: DCert crypto -> Bool
isDeRegKey (DCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for 'Delegation' constructor
isDelegation :: DCert crypto -> Bool
isDelegation (DCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: DCert crypto -> Bool
isGenesisDelegation (DCertGenesis GenesisDelegCert {}) = True
isGenesisDelegation _ = False

-- | Check for 'VestedDelegate' constructor 
isVestedDelegation :: DCert crypto -> Bool
isVestedDelegation (DCertVested VestedDelegCert {}) = True
isVestedDelegation _= False

-- | Check for 'RegPool' constructor
isRegPool :: DCert crypto -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: DCert crypto -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

isInstantaneousRewards :: DCert crypto -> Bool
isInstantaneousRewards (DCertMir _) = True
isInstantaneousRewards _ = False

isReservesMIRCert :: DCert crypto -> Bool
isReservesMIRCert (DCertMir (MIRCert ReservesMIR _)) = True
isReservesMIRCert _ = False

isTreasuryMIRCert :: DCert crypto -> Bool
isTreasuryMIRCert (DCertMir (MIRCert TreasuryMIR _)) = True
isTreasuryMIRCert _ = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
requiresVKeyWitness :: DCert crypto -> Bool
requiresVKeyWitness (DCertMir (MIRCert _ _)) = False
requiresVKeyWitness (DCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True

-- Deprecated

{-# DEPRECATED PoolDistr "Import from Bcc.Protocol.TOptimum instead" #-}

type PoolDistr = TP.PoolDistr

{-# DEPRECATED IndividualPoolStake "Import from Bcc.Protocol.TOptimum instead" #-}

type IndividualPoolStake = TP.IndividualPoolStake