{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SimHash
  ( train
  , test
  , SimHashModel
  , loadModel

  , RunnerQueue
  , newRunnerQueue
  , readRunnerQueue
  , startRunner
  , startSaver

  , inferOne
  , inferTask
  , inferLearnTask
  ) where


import           Control.Monad        (forM_, forever, unless, void, when)
import           Data.Aeson           (encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Int             (Int64)
import           Data.List            (elemIndex, sortBy)
import           Data.Text            (Text)
import qualified Data.Text            as T (drop, intercalate, pack, split,
                                            strip, take)
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.Text.IO         as T (readFile, writeFile)
import           Htm.SimHash
import           Htm.Stats
import           Htm.Utils
import           Metro.Utils          (getEpochTime)
import           Periodic.Job         (JobM, workDone, workDone_, workload)
import           System.Directory     (doesFileExist, renameFile)
import           UnliftIO             (Async, MonadIO, TMVar, TQueue, TVar,
                                       async, atomically, modifyTVar',
                                       newEmptyTMVarIO, newTQueueIO, newTVarIO,
                                       readTQueue, readTVar, readTVarIO,
                                       registerDelay, retrySTM, takeTMVar,
                                       tryPutTMVar, writeTQueue, writeTVar)

data SimHashModel = SimHashModel
  { modelLabels :: TVar [Text]
  , modelFile   :: FilePath
  , model       :: SimHash
  }


loadModel :: FilePath -> IO SimHashModel
loadModel modelFile = do
  model <- new
  exists0 <- doesFileExist modelFile
  exists1 <- doesFileExist $ modelFile ++ ".sp"
  unless (exists0 || exists1) $ setup model
  modelLabels <- newTVarIO []

  when (exists0 || exists1) $ do
    labels <- T.split (=='\n') . T.strip <$> T.readFile (modelFile ++ ".labels")
    atomically $ writeTVar modelLabels labels

  when exists0 $ loadFromFile (encodeUtf8 $ T.pack modelFile) model
  when exists1 $ loadFromFileV2 spFile clsrFile model

  pure SimHashModel {..}

  where spFile = encodeUtf8 $ T.pack $ modelFile ++ ".sp"
        clsrFile = encodeUtf8 $ T.pack $ modelFile ++ ".clsr"


saveModel :: SimHashModel -> IO ()
saveModel SimHashModel {..} = do
  saveToFile (encodeUtf8 $ T.pack $ modelFile ++ ".1") model
  labels <- readTVarIO modelLabels
  T.writeFile (modelFile ++ ".labels.1") $ T.intercalate "\n" labels
  renameFile (modelFile ++ ".1") modelFile
  renameFile (modelFile ++ ".labels.1") (modelFile ++ ".labels")

train :: SimHashModel -> Stats -> FilePath -> IO Stats
train shm@SimHashModel {..} stats dataFile = do
  countH <- newTVarIO 0
  startedAt <- getEpochTime
  readLineAndDo dataFile $ \str label -> do
    idx <- getLabelIdx modelLabels label
    learn str idx model
    count <- atomically $ do
      c <- readTVar countH
      writeTVar countH $! c + 1
      pure c

    when (count `mod` 1024 == 0) $ showTrainStats startedAt count

  count <- readTVarIO countH
  showTrainStats startedAt count
  finishedAt <- getEpochTime

  saveModel shm

  pure stats
    { trainCount = count
    , trainStartedAt = startedAt
    , trainFinishedAt = finishedAt
    , trainSpent = prettyTime (finishedAt - startedAt)
    }

  where showTrainStats :: Int64 -> Int -> IO ()
        showTrainStats startedAt count = do
          now <- getEpochTime
          putStrLn $ "Train iters " ++ show count
          putStrLn $ "Train spent " ++ prettyTime (now - startedAt)


test :: SimHashModel -> Stats -> FilePath -> IO Stats
test SimHashModel {..} stats testFile = do
  totalH <- newTVarIO 0
  rightH <- newTVarIO 0
  labels <- readTVarIO modelLabels
  let size = length labels
  startedAt <- getEpochTime

  readLineAndDo testFile $ \str label -> do
    infers <- infer str size model
    atomically $ do
      modifyTVar' totalH (+1)
      case elemIndex label labels of
        Nothing -> pure ()
        Just idx ->
          when (argmax infers == idx) $ modifyTVar' rightH (+1)

    total <- readTVarIO totalH
    when (total `mod` 1024 == 0) $ showTestStats startedAt totalH rightH

  showTestStats startedAt totalH rightH
  finishedAt <- getEpochTime
  right <- fromIntegral <$> readTVarIO rightH
  total <- fromIntegral <$> readTVarIO totalH

  pure stats
    { testCount = total
    , testStartedAt = startedAt
    , testFinishedAt = finishedAt
    , testScore = floor (fromIntegral right * 10000 / fromIntegral total)
    , testSpent = prettyTime (finishedAt - startedAt)
    }

  where showTestStats :: Int64 -> TVar Int -> TVar Int -> IO ()
        showTestStats startedAt totalH rightH = do
          now <- getEpochTime
          right <- readTVarIO rightH
          total <- readTVarIO totalH
          putStrLn $ "Test iters " ++ show total
          putStrLn $ "Test score " ++ show (fromIntegral right / fromIntegral total)
          putStrLn $ "Test spent " ++ prettyTime (now - startedAt)


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
  { runnerModel :: SimHashModel
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


  where labelTVar = modelLabels runnerModel
        m = model runnerModel
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
