{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , AdaPots (..)
  , allegraEpochUpdate
  , epochUpdate
  , maryEpochUpdate
  , shelleyEpochUpdate
  ) where

import           Cardano.Prelude

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Era.Shelley.Generic.ProtoParams
import           Cardano.Sync.Era.Shelley.Generic.Rewards
import           Cardano.Sync.Era.Shelley.Generic.StakeDist
import           Cardano.Sync.Types

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))

data NewEpoch = NewEpoch
  { epoch :: !EpochNo
  , isEBB :: !Bool
  , adaPots :: !(Maybe AdaPots)
  , neProtoParams :: !ProtoParams
  , neNonce :: !Shelley.Nonce
  , neEpochUpdate :: !(Maybe EpochUpdate)
  }

data EpochUpdate = EpochUpdate
  { euEpoch :: !EpochNo
  , euRewards :: !(Maybe Rewards)
  , euStakeDistribution :: !StakeDist
  }

-- There is a similar type in ledger-spec, but it is not exported yet.
data AdaPots = AdaPots
  { apTreasury :: !Coin
  , apReserves :: !Coin
  , apRewards :: !Coin
  , apUtxo :: !Coin
  , apDeposits :: !Coin
  , apFees :: !Coin
  }

-- Create an EpochUpdate from the current epoch state and the rewards from the last epoch.
epochUpdate :: Shelley.Network -> EpochNo -> ExtLedgerState CardanoBlock -> Maybe Rewards -> Maybe EpochUpdate
epochUpdate nw epochNo els mRewards =
  case ledgerState els of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Just $ shelleyEpochUpdate nw epochNo sls mRewards
    LedgerStateAllegra als -> Just $ allegraEpochUpdate nw epochNo als mRewards
    LedgerStateMary mls -> Just $ maryEpochUpdate nw epochNo mls mRewards



allegraEpochUpdate :: Shelley.Network -> EpochNo -> LedgerState (ShelleyBlock StandardAllegra) -> Maybe Rewards -> EpochUpdate
allegraEpochUpdate nw epochNo als mRewards =
  EpochUpdate
    { euEpoch = epochNo
    , euRewards = mRewards
    , euStakeDistribution = allegraStakeDist nw als
    }

maryEpochUpdate :: Shelley.Network -> EpochNo -> LedgerState (ShelleyBlock StandardMary) -> Maybe Rewards -> EpochUpdate
maryEpochUpdate nw epochNo mls mRewards =
  EpochUpdate
    { euEpoch = epochNo
    , euRewards = mRewards
    , euStakeDistribution = maryStakeDist nw mls
    }

shelleyEpochUpdate :: Shelley.Network -> EpochNo -> LedgerState (ShelleyBlock StandardShelley) -> Maybe Rewards -> EpochUpdate
shelleyEpochUpdate nw epochNo sls mRewards =
  EpochUpdate
    { euEpoch = epochNo
    , euRewards = mRewards
    , euStakeDistribution = shelleyStakeDist nw sls
    }
