{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Insert.Epoch
  ( runEpochUpdateThread
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

-- import           Cardano.Db (DbLovelace (..), DbWord64 (..))

import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Query
-- import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
-- import           Cardano.DbSync.Era.Shelley.Query
import           Cardano.DbSync.Era.Util
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
-- import           Cardano.Sync.Types
import           Cardano.Sync.Util

-- import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import           Control.Concurrent.STM.TVar (writeTVar)
-- import           Control.Monad.Trans.Control (MonadBaseControl)

import           Control.Monad.Extra (whenJust)
-- import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)

-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy.Char8 as LBS
-- import           Data.Group (invert)
import           Data.List.Split.Internals (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- import qualified Data.Text as Text

import           Database.Persist.Sql (SqlBackend, putMany)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley


-- Here we insert data that only changes on epoch boundaries, like epoch rewards
-- and the updated stake distribution. These are calculated 48 hours after the
-- start of the previous epoch. They used to be inserted in the database at
-- the new epoch, but this operation (synchronous) was taking 6 minutes, which
-- is significantly too long.

-- The idea now is to grab the relevant data when it is first calculated, start
-- a database transaction, insert all the data, but only commit the transaction
-- when the epoch rolls over. All the synchronisation is done using `TMVar`s
-- and `TVar`s.

-- This function runs forever in a separate thread and the EpochUpdate data to
-- be inserted is passed via a `TMVar` and another `TMVar` is used to signal the
-- main insert thread of completion.

runEpochUpdateThread :: Trace IO Text -> EpochUpdateControl -> IO ()
runEpochUpdateThread tracer euc = do
  logInfo tracer "runEpochUpdateThread"
  forever $ do
    logInfo tracer "Waiting for EpochUpdate"

    -- Will block until data arrives.
    epochUpdate <- atomically $ takeTMVar (ruInsertDone euc)

    logInfo tracer $
        mconcat
          [ "Asynchonously inserting epoch updates for epoch "
          , textShow (unEpochNo (Generic.euEpoch epochUpdate) - 2)
          ]
    -- This starts a new database connection and runs the following in a
    -- transaction.
    DB.runDbNoLogging $ do
      res <- runExceptT $ insertEpochUpdate tracer epochUpdate
      case res of
        Left err -> liftIO . logInfo tracer $ "insertEpochUpdate: " <> renderSyncNodeError err
        Right () -> pure ()
      liftIO $ waitOnTMVar (Generic.euEpoch epochUpdate)

    reportEpochUpdate tracer epochUpdate
  where
    waitOnTMVar :: EpochNo -> IO ()
    waitOnTMVar epochNo = do
      -- Signal the main thread that insertion is complete.
      atomically $ do
        writeTVar (ruState euc) (WaitingForEpoch $ epochNo + 1)
        putTMVar (ruUpdateReady euc) ()

      logInfo tracer $
        mconcat
          [ "Asynchonous insert for epoch "
          , textShow (unEpochNo epochNo - 2)
          , " done, waiting for epoch boundary"
          ]

      -- Wait for the main thread to notify us that its time to commit the transaction.
      atomically $ do
        takeTMVar (ruCommit euc)
        writeTVar (ruState euc) WaitingForData
      logInfo tracer $
        mconcat
          [ "Committing insert for epoch "
          , textShow (unEpochNo epochNo - 2)
          , " done"
          ]

-- -------------------------------------------------------------------------------------------------

insertEpochUpdate
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Generic.EpochUpdate
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochUpdate tracer eu = do
  whenJust (Generic.euRewards eu) $ \ grewards -> do
    insertRewards tracer (Generic.euEpoch eu) (Generic.rewards grewards)
    insertOrphanedRewards tracer (Generic.euEpoch eu) (Generic.orphaned grewards)
  insertEpochStake tracer (Generic.euEpoch eu) (Generic.euStakeDistribution eu)


insertEpochStake
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Generic.StakeDist
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake _tracer (EpochNo epochNo) smap =
    forM_ (chunksOf 1000 $ Map.toList (Generic.unStakeDist smap)) $ \stakeChunk -> do
      dbStakes <- mapM mkStake stakeChunk
      lift $ putMany dbStakes
  where
    mkStake
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Shelley.Coin)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake (saddr, coin) = do
      (saId, poolId) <- liftLookupFail "insertEpochStake" $ queryStakeAddressAndPool epochNo (Generic.unStakeCred saddr)
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = epochNo -- The epoch where this delegation becomes valid.
          }

insertRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards _tracer epoch rewards = do
    forM_ (chunksOf 1000 $ Map.toList rewards) $ \rewardsChunk -> do
      dbRewards <- concatMapM mkRewards rewardsChunk
      lift $ putMany dbRewards
  where
    mkRewards
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- liftLookupFail "insertReward StakeAddress" $ queryStakeAddress (Generic.unStakeCred saddr)
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- liftLookupFail "insertReward StakePool" $ queryStakePoolKeyHash (Shelley.rewardPool rwd)
        pure $ DB.Reward
                  { DB.rewardAddrId = saId
                  , DB.rewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.rewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.rewardEpochNo = unEpochNo epoch
                  , DB.rewardPoolId = poolId
                  }

insertOrphanedRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOrphanedRewards _tracer epoch orphanedRewards =
    -- There are probably not many of these for each epoch, but just in case there
    -- are, it does not hurt to chunk them.
    forM_ (chunksOf 1000 $ Map.toList orphanedRewards) $ \orphanedRewardsChunk -> do
      dbRewards <- concatMapM mkOrphanedReward orphanedRewardsChunk
      lift $ putMany dbRewards
  where
    mkOrphanedReward
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.OrphanedReward]
    mkOrphanedReward (saddr, rset) = do
      saId <- liftLookupFail "insertReward StakeAddress" $ queryStakeAddress (Generic.unStakeCred saddr)
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- liftLookupFail "insertReward StakePool" $ queryStakePoolKeyHash (Shelley.rewardPool rwd)
        pure $ DB.OrphanedReward
                  { DB.orphanedRewardAddrId = saId
                  , DB.orphanedRewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.orphanedRewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.orphanedRewardEpochNo = unEpochNo epoch
                  , DB.orphanedRewardPoolId = poolId
                  }

reportEpochUpdate :: Trace IO Text -> Generic.EpochUpdate -> IO ()
reportEpochUpdate tracer eu =
    logInfo tracer $
      mconcat
        [ "Finishing epoch ", textShow (unEpochNo $ Generic.euEpoch eu), ": "
        , textShow rewardCount, " rewards, ", textShow orphanedCount, " orphaned_rewards, "
        , textShow (length (Generic.unStakeDist $ Generic.euStakeDistribution eu)), " stakes."
        ]
  where
    rewardCount :: Int
    rewardCount = maybe 0 length (Generic.rewards <$> Generic.euRewards eu)

    orphanedCount :: Int
    orphanedCount = maybe 0 length (Generic.orphaned  <$> Generic.euRewards eu)
