module Bcc.Chain.UTxO.GenesisUTxO
  ( genesisUtxo,
  )
where

import Bcc.Chain.Common (Address, Entropic, makeRedeemAddress)
import Bcc.Chain.Common.NetworkMagic (NetworkMagic, makeNetworkMagic)
import Bcc.Chain.Genesis (unGenesisAvvmBalances, unGenesisNonAvvmBalances)
import qualified Bcc.Chain.Genesis as Genesis
import Bcc.Chain.UTxO.UTxO (UTxO)
import qualified Bcc.Chain.UTxO.UTxO as UTxO
import Bcc.Crypto (fromCompactRedeemVerificationKey)
import Bcc.Prelude
import qualified Data.Map.Strict as M

-- | Create initial 'UTxO' from balances defined in the genesis config
genesisUtxo :: Genesis.Config -> UTxO
genesisUtxo config = UTxO.fromBalances balances
  where
    balances :: [(Address, Entropic)]
    balances = avvmBalances <> nonAvvmBalances

    avvmBalances :: [(Address, Entropic)]
    avvmBalances =
      first (makeRedeemAddress networkMagic . fromCompactRedeemVerificationKey)
        <$> M.toList (unGenesisAvvmBalances $ Genesis.configAvvmDistr config)

    networkMagic :: NetworkMagic
    networkMagic = makeNetworkMagic (Genesis.configProtocolMagic config)

    nonAvvmBalances :: [(Address, Entropic)]
    nonAvvmBalances =
      M.toList $ unGenesisNonAvvmBalances $ Genesis.configNonAvvmBalances config
