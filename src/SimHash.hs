{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SimHash
  ( RunnerQueue
  , newRunnerQueue
  , readRunnerQueue
  , startRunner
  , startSaver

  , inferOne
  , inferTask
  , inferLearnTask
  ) where


import           Control.Monad        (forM_, forever, unless, void)
import           Data.Aeson           (encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.List            (sortBy)
import           Data.Text            (Text)
import qualified Data.Text            as T (drop, take)
import           Data.Text.Encoding   (encodeUtf8)
import           Htm.Model
import           Htm.SimHash
import           Htm.Utils
import           Periodic.Job         (JobM, workDone, workDone_, workload)
import           UnliftIO             (Async, MonadIO, TMVar, TQueue, TVar,
                                       async, atomically, newEmptyTMVarIO,
                                       newTQueueIO, newTVarIO, readTQueue,
                                       readTVar, readTVarIO, registerDelay,
                                       retrySTM, takeTMVar, tryPutTMVar,
                                       writeTQueue, writeTVar)


data QueueItem = QueueItem
  { itemMsg   :: ByteString
  , itemRet   :: Maybe (TMVar [(Text, Double)])
  , itemLabel :: Maybe Text
  }


type RunnerQueue = TQueue QueueItem

newRunnerQueue :: IO (TQueue RunnerQueue)
newRunnerQueue = newTQueueIO


readRunnerQueue :: MonadIO m => TQueue RunnerQueue -> m RunnerQueue
readRunnerQueue queues = do
  queue <- atomically $ readTQueue queues
  atomically $ writeTQueue queues queue
  pure queue


data Runner = Runner
  { runnerModel :: Model
  , runnerQueue :: RunnerQueue
  , runnerSaver :: TVar (Maybe (TVar Bool))
  }


newRunner :: RunnerQueue -> FilePath -> IO Runner
newRunner runnerQueue path = do
  runnerModel <- loadModel path
  runnerSaver <- newTVarIO Nothing
  pure Runner {..}


runRunner :: Runner -> IO ()
runRunner Runner {..} = do
  QueueItem {..} <- atomically $ readTQueue runnerQueue
  forM_ itemRet $ \ret -> do
    labels <- readTVarIO labelTVar
    let size = length labels
    infers <- infer itemMsg size m
    atomically
      . void
      . tryPutTMVar ret
      $! take 10
      . sortBy (\(_, a) (_, b) -> compare b a)
      $ zip labels infers

  forM_ itemLabel $ \label -> do
    idx <- getLabelIdx labelTVar label
    learn itemMsg idx m

    doSave <- registerDelay delayUS
    atomically $ writeTVar runnerSaver $ Just doSave


  where labelTVar = labelHandle runnerModel
        m = simhash runnerModel
        delayUS = 60000000 -- 60s


startRunner :: TQueue RunnerQueue -> FilePath -> IO (Runner, Async ())
startRunner tqueue path = do
  queue <- newTQueueIO
  atomically $ writeTQueue tqueue queue
  runner <- newRunner queue path
  io <- async $ forever $ runRunner runner
  pure (runner, io)


startSaver :: Runner -> IO (Async ())
startSaver Runner {..} = async $ forever $ do
  atomically $ do
    mSaver <- readTVar runnerSaver
    case mSaver of
      Nothing -> retrySTM
      Just saver -> do
        doSave <- readTVar saver
        unless doSave retrySTM

  saveModel runnerModel


inferOne :: MonadIO m => RunnerQueue -> ByteString -> m [(Text, Double)]
inferOne queue itemMsg = do
  iRet <- newEmptyTMVarIO
  atomically $ writeTQueue queue QueueItem
    { itemLabel = Nothing
    , itemRet = Just iRet
    , ..
    }
  atomically $ takeTMVar iRet


inferTask :: TQueue RunnerQueue -> JobM ()
inferTask queues = do
  msg <- workload
  queue <- readRunnerQueue queues
  ret <- inferOne queue msg
  workDone_ $ toStrict $ encode ret


doInferLearnTask :: Text -> Text -> RunnerQueue -> JobM ()
doInferLearnTask "0" msg queue = do
  iRet    <- newEmptyTMVarIO
  atomically $ writeTQueue queue QueueItem
    { itemLabel = Nothing
    , itemRet = Just iRet
    , itemMsg = encodeUtf8 msg
    }
  ret <- atomically $ takeTMVar iRet
  workDone_ $ toStrict $ encode ret

doInferLearnTask "1" msg queue = do
  atomically $ writeTQueue queue QueueItem
    { itemLabel = Just label
    , itemRet   = Nothing
    , itemMsg   = encodeUtf8 str
    }
  workDone
  where (label, str) = splitLabelAndMsg msg

doInferLearnTask _ msg queue = do
  iRet    <- newEmptyTMVarIO
  atomically $ writeTQueue queue QueueItem
    { itemLabel = Just label
    , itemRet   = Just iRet
    , itemMsg   = encodeUtf8 str
    }
  ret <- atomically $ takeTMVar iRet
  workDone_ $ toStrict $ encode ret
  where (label, str) = splitLabelAndMsg msg


-- 0msg         only infer
-- 1label,msg   only learn
-- 2label,msg   learn and infer
inferLearnTask :: RunnerQueue -> JobM ()
inferLearnTask queue = do
  msg <- workload
  doInferLearnTask (T.take 1 msg) (T.drop 1 msg) queue
