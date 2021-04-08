{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock)
import           Cardano.DbSync.Rollback (rollbackToSlot)

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Api
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Plugin
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Control.Concurrent.STM.TMVar (putTMVar)
import           Control.Concurrent.STM.TVar (readTVarIO, writeTVar)
import           Control.Monad.Logger (LoggingT)

import qualified Data.List as List
import qualified Data.Map.Strict as Map

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

-- | The default SyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: SqlBackend -> SyncNodePlugin
defDbSyncNodePlugin backend =
  SyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock backend]
    , plugRollbackBlock = [rollbackToSlot backend]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: SqlBackend -> Trace IO Text -> SyncEnv -> [BlockDetails]
    -> IO (Either SyncNodeError ())
insertDefaultBlock backend tracer env blockDetails = do
    -- Take the list of BlockDetails, split the list into chunks that only contain the same
    -- epoch and then insert the chunks.
    traverseMEither insertEpochChunk $ chunkByEpoch blockDetails
  where
    insertEpochChunk :: [BlockDetails] -> IO (Either SyncNodeError ())
    insertEpochChunk xs =
      case xs of
        [] -> pure $ Right ()
        (x:_) -> do
          commitEpochUpdate tracer (envLedger env) (sdEpochNo $ bdSlot x)
          res <- DB.runDbIohkLogging backend tracer $
                    traverseMEither insertBlock xs
          logInfo tracer "insertEpochChunk done"
          pure res

    insertBlock :: BlockDetails -> ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
    insertBlock (BlockDetails cblk details) = do
      -- Calculate the new ledger state to pass to the DB insert functions but do not yet
      -- update ledgerStateVar.
      let network = leNetwork (envLedger env)
      lStateSnap <- liftIO $ applyBlock (envLedger env) cblk
      prepareEpochUpdate (envLedger env)
      res <- case cblk of
                BlockByron blk ->
                  insertByronBlock tracer blk details
                BlockShelley blk ->
                  insertShelleyBlock tracer network (Generic.fromShelleyBlock blk) lStateSnap details
                BlockAllegra blk ->
                  insertShelleyBlock tracer network (Generic.fromAllegraBlock blk) lStateSnap details
                BlockMary blk ->
                  insertShelleyBlock tracer network (Generic.fromMaryBlock blk) lStateSnap details
      -- Now we update it in ledgerStateVar and (possibly) store it to disk.
      liftIO $ saveLedgerStateMaybe (envLedger env)
                    lStateSnap (isSyncedWithinSeconds details 60)
      pure res

commitEpochUpdate :: Trace IO Text -> LedgerEnv -> EpochNo -> IO ()
commitEpochUpdate tracer env currentEpoch = do
  st <- readTVarIO (ruState $ leEpochUpdate env)
  logInfo tracer $ "commitEpochUpdate: " <> textShow st
  case st of
    WaitingForData -> pure ()
    Processing -> pure ()
    WaitingForEpoch waitEpoch -> do
      logInfo tracer $ "commitEpochUpdate: " <> textShow (waitEpoch, currentEpoch)
      when (waitEpoch == currentEpoch) $
        atomically $ putTMVar (ruCommit $ leEpochUpdate env) ()


chunkByEpoch :: [BlockDetails] -> [[BlockDetails]]
chunkByEpoch ws =
  case ws of
      [] -> []
      xs@(xh:_) ->
        -- Parition is fine here because the list is already ordered, so partioning by
        -- the epoch number will retain the ordering.
        case List.partition (\x -> sdEpochNo (bdSlot x) == sdEpochNo (bdSlot xh)) xs of
          ([], _) -> panic "Cardano.DbSync.Plugin.Default.chunkByEpoch: Impossible"
          (ys, []) -> [ys]
          (ys, zs) -> ys : chunkByEpoch zs

prepareEpochUpdate :: MonadIO m => LedgerEnv -> ReaderT SqlBackend m ()
prepareEpochUpdate env = do
    currState <- liftIO $ readTVarIO (leStateVar env)
    upState <- liftIO $ readTVarIO (ruState $ leEpochUpdate env)
    when (upState /= WaitingForData) $ do
      let mRewards = Generic.epochRewards (leNetwork env) (clsState currState)
      case Generic.epochUpdate (leNetwork env) (ledgerEpochNo env currState) (clsState currState) mRewards of
        Nothing -> pure ()
        Just origRu -> do
          ru <- queryPopulateEpochUpdateCaches env origRu
          liftIO . atomically $ do
            putTMVar (ruInsertDone $ leEpochUpdate env) ru
            writeTVar (ruState $ leEpochUpdate env) Processing

queryPopulateEpochUpdateCaches :: MonadIO m => SyncEnv -> Generic.EpochUpdate -> ReaderT SqlBackend m Generic.EpochUpdate
queryPopulateEpochUpdateCaches env origRu = do
    addrs <- mapM ((\ a -> (a,) <$> queryStakeAddress) . Generic.stakingCredHash env) stakeAddresses
    pure $ origRu
            { Generic.enStakeAddressCache = Map.fromList addrs
            }
  where
    stakeAddresses :: [Generic.StakeCred]
    stakeAddresses = List.nub . List.sort $ Map.keys (Generic.enStakeAddressCache origRu)
