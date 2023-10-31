{-# LANGUAGE RecordWildCards #-}

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
import           Data.Text       (Text)
import           Htm.Model       (Model (..), infer)
import           UnliftIO        (Async, MonadIO, TMVar, TQueue, TVar, async,
                                  atomically, newEmptyTMVarIO, newTQueueIO,
                                  newTVarIO, readTQueue, readTVar,
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


newRunner :: Queue -> IO Model -> IO Runner
newRunner runnerQueue loadModel = do
  runnerModel <- loadModel
  runnerSaver <- newTVarIO Nothing
  pure Runner {..}


runRunner :: Runner -> IO ()
runRunner Runner {..} = do
  Item {..} <- atomically $ readTQueue runnerQueue
  forM_ itemRet $ \ret -> do
    infers <- infer itemMsg runnerModel
    void $ atomically . tryPutTMVar ret $! take 10 infers

  forM_ itemLabel $ \label -> do
    modelLearn runnerModel itemMsg label
    doSave <- registerDelay delayUS
    atomically $ writeTVar runnerSaver $ Just doSave


  where delayUS = 60000000 -- 60s


startRunner :: Queue -> IO Model -> IO (Runner, Async ())
startRunner queue loadModel = do
  runner <- newRunner queue loadModel
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

  modelSave runnerModel
  atomically $ writeTVar runnerSaver Nothing


inferOne :: MonadIO m => Queue -> ByteString -> m [(Text, Double)]
inferOne queue itemMsg = do
  iRet <- newEmptyTMVarIO
  atomically $ writeTQueue queue Item
    { itemLabel = Nothing
    , itemRet = Just iRet
    , ..
    }
  atomically $ takeTMVar iRet
