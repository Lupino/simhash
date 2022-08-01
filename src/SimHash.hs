{-# LANGUAGE OverloadedStrings #-}

module SimHash
  ( newQueues
  , readQueue
  , newRQueue

  , inferTask
  , inferLearnTask
  ) where


import           Data.Aeson           (encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text            (Text)
import qualified Data.Text            as T (drop, take)
import           Data.Text.Encoding   (encodeUtf8)
import           Htm.Runner           (Item (..), Queue, inferOne)
import           Htm.Utils            (splitLabelAndMsg)
import           Periodic.Job         (JobM, workDone, workDone_, workload)
import           UnliftIO             (MonadIO, TQueue, atomically,
                                       newEmptyTMVarIO, newTQueueIO, readTQueue,
                                       takeTMVar, writeTQueue)


newQueues :: IO (TQueue Queue)
newQueues = newTQueueIO


newRQueue :: TQueue Queue -> IO Queue
newRQueue queues = do
  queue <- newTQueueIO
  atomically $ writeTQueue queues queue
  pure queue


readQueue :: MonadIO m => TQueue Queue -> m Queue
readQueue queues = do
  queue <- atomically $ readTQueue queues
  atomically $ writeTQueue queues queue
  pure queue


inferTask :: TQueue Queue -> JobM ()
inferTask queues = do
  msg <- workload
  queue <- readQueue queues
  ret <- inferOne queue msg
  workDone_ $ toStrict $ encode ret


doInferLearnTask :: Text -> Text -> Queue -> JobM ()
doInferLearnTask "0" msg queue = do
  iRet    <- newEmptyTMVarIO
  atomically $ writeTQueue queue Item
    { itemLabel = Nothing
    , itemRet = Just iRet
    , itemMsg = encodeUtf8 msg
    }
  ret <- atomically $ takeTMVar iRet
  workDone_ $ toStrict $ encode ret

doInferLearnTask "1" msg queue = do
  atomically $ writeTQueue queue Item
    { itemLabel = Just label
    , itemRet   = Nothing
    , itemMsg   = encodeUtf8 str
    }
  workDone
  where (label, str) = splitLabelAndMsg msg

doInferLearnTask _ msg queue = do
  iRet    <- newEmptyTMVarIO
  atomically $ writeTQueue queue Item
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
inferLearnTask :: Queue -> JobM ()
inferLearnTask queue = do
  msg <- workload
  doInferLearnTask (T.take 1 msg) (T.drop 1 msg) queue
