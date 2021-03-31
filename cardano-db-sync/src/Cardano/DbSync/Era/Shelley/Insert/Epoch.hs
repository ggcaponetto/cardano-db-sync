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
-- import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
-- import           Cardano.DbSync.Era.Shelley.Query

import           Cardano.Sync.LedgerState
-- import           Cardano.Sync.Types
import           Cardano.Sync.Util

-- import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import           Control.Concurrent.STM.TVar (writeTVar)
-- import           Control.Monad.Trans.Control (MonadBaseControl)

-- import           Control.Monad.Extra (whenJust)
-- import           Control.Monad.Logger (LoggingT)
-- import           Control.Monad.Trans.Control (MonadBaseControl)

-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy.Char8 as LBS
-- import           Data.Group (invert)
-- import           Data.List.Split.Internals (chunksOf)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import qualified Data.Text as Text

import           Database.Persist.Sql (SqlBackend)



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
    -- Will block until data arrives.
    epochUpdate <- atomically $ takeTMVar (ruInsertDone euc)

    liftIO . logInfo tracer $
        mconcat
          [ "Asynchonously inserting epoch updates for epoch "
          , textShow (unEpochNo $ Generic.euEpoch epochUpdate)
          ]
    -- This starts a new database connection and runs the following in a
    -- transaction.
    DB.runDbNoLogging $ do
    -- DB.runIohkLogging tracer $
      -- withPostgresqlConn connectString $ \ backend ->
        -- DB.runDbIohkLogging backend tracer $ do
      insertEpochUpdate tracer epochUpdate
      liftIO $ waitOnTMVar (Generic.euEpoch epochUpdate)

  where
    waitOnTMVar :: EpochNo -> IO ()
    waitOnTMVar epochNo = do
      -- Signal the main thread that insertion is complete.
      atomically $ do
        writeTVar (ruState euc) WaitingForEpoch
        putTMVar (ruUpdateReady euc) ()

      logInfo tracer $
        mconcat
          [ "Asynchonous insert for epoch "
          , textShow (unEpochNo epochNo)
          , " done, waiting for epoch boundary"
          ]

      -- Wait for the main thread to notify us that its time to commit the transaction.
      void . atomically $ takeTMVar (ruCommit euc)
      logInfo tracer $
        mconcat
          [ "Committing insert for epoch "
          , textShow (unEpochNo epochNo)
          , " done"
          ]

insertEpochUpdate
    :: MonadIO m -- (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Generic.EpochUpdate
    -> ReaderT SqlBackend m ()
insertEpochUpdate tracer _eu =
  liftIO $ logInfo tracer "insertEpochUpdate"
