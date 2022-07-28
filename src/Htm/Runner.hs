{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.Runner
  ( Queue
  , Item  (..)
  , newQueue
  , startRunner
  , startSaver

  , inferOne
  ) where


import           Control.Monad   (forM_, forever, unless, void)
import           Data.ByteString (ByteString)
import           Data.List       (sortBy)
import           Data.Text       (Text)
import           Htm.Model       (Model (..), loadModel, saveModel)
import           Htm.SimHash     (infer, learn)
import           Htm.Utils       (getLabelIdx)
import           UnliftIO        (Async, MonadIO, TMVar, TQueue, TVar, async,
                                  atomically, newEmptyTMVarIO, newTQueueIO,
                                  newTVarIO, readTQueue, readTVar, readTVarIO,
                                  registerDelay, retrySTM, takeTMVar,
                                  tryPutTMVar, writeTQueue, writeTVar)


data Item = Item
  { itemMsg   :: ByteString
  , itemRet   :: Maybe (TMVar [(Text, Double)])
  , itemLabel :: Maybe Text
  }


type Queue = TQueue Item

newQueue :: IO Queue
newQueue = newTQueueIO

data Runner = Runner
  { runnerModel :: Model
  , runnerQueue :: Queue
  , runnerSaver :: TVar (Maybe (TVar Bool))
  }


newRunner :: Queue -> FilePath -> IO Runner
newRunner runnerQueue path = do
  runnerModel <- loadModel path
  runnerSaver <- newTVarIO Nothing
  pure Runner {..}


runRunner :: Runner -> IO ()
runRunner Runner {..} = do
  Item {..} <- atomically $ readTQueue runnerQueue
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


startRunner :: Queue -> FilePath -> IO (Runner, Async ())
startRunner queue path = do
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


inferOne :: MonadIO m => Queue -> ByteString -> m [(Text, Double)]
inferOne queue itemMsg = do
  iRet <- newEmptyTMVarIO
  atomically $ writeTQueue queue Item
    { itemLabel = Nothing
    , itemRet = Just iRet
    , ..
    }
  atomically $ takeTMVar iRet
