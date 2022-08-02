{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.Model
  ( Model (..)
  , infer
  , trainAndValid

  , loadLabels
  , saveLabels
  ) where


import           Control.Monad    (replicateM_, unless, when)
import           Data.ByteString  (ByteString)
import           Data.Int         (Int64)
import           Data.List        (sortBy)
import           Data.Text        (Text)
import qualified Data.Text        as T (intercalate, split, strip)
import qualified Data.Text.IO     as T (readFile, writeFile)
import           Htm.Stats        (Stats (..), saveStatsToFile)
import           Htm.Utils        (argmax, getLabelIdx, prettyTime,
                                   readLineAndDo, whenExists)
import           Metro.Utils      (getEpochTime)
import           System.Directory (renameFile)
import           UnliftIO         (TVar, atomically, modifyTVar', newTVarIO,
                                   readTVarIO, writeTVar)

data Model = Model
  { modelLabels :: TVar [Text]
  , modelSave   :: IO ()
  , modelLearn  :: ByteString -> Text -> IO ()
  , modelInfer  :: ByteString -> Int -> IO [Double]
  }


infer :: ByteString -> Model -> IO [(Text, Double)]
infer str Model {..} = do
  labels <- readTVarIO modelLabels
  infers <- modelInfer str (length labels)
  pure $ sortBy (\(_, a) (_, b) -> compare b a) $ zip labels infers


countSample :: FilePath -> IO Int
countSample dataFile = do
  v <- newTVarIO 0
  readLineAndDo dataFile $ \_ _ ->
    atomically $ modifyTVar' v (+1)

  readTVarIO v



train :: Model -> FilePath -> (Bool -> IO ()) -> IO ()
train Model {..} trainFile showProc = do
  readLineAndDo trainFile $ \str label -> do
    modelLearn str label
    showProc False

  modelSave
  showProc True


test :: Model -> FilePath -> TVar Int -> (Bool -> IO ()) -> IO ()
test Model {..} testFile validH showProc = do
  size <- length <$> readTVarIO modelLabels

  readLineAndDo testFile $ \str label -> do
    infers <- modelInfer str size
    idx <- getLabelIdx modelLabels label

    when (argmax infers == idx) $ atomically $ modifyTVar' validH (+1)
    showProc False

  showProc True


resetProcced :: TVar Int -> IO ()
resetProcced proccedH = atomically $ writeTVar proccedH 0


trainAndValidOne
  :: Model -> FilePath -> FilePath
  -> IO ()
  -> (Int64 -> Bool -> IO ())
  -> (Maybe (TVar Int) -> Int64 -> Bool -> IO ())
  -> IO Stats
trainAndValidOne model@Model {..} trainFile validFile reset showTrainProc showTestProc = do
  trainStartedAt <- getEpochTime

  reset
  train model trainFile
    $ showTrainProc trainStartedAt

  testStartedAt <- getEpochTime
  testValidH    <- newTVarIO 0

  reset
  test model validFile testValidH
    $ showTestProc (Just testValidH) testStartedAt

  validCount <- readTVarIO testValidH

  testFinishedAt <- getEpochTime
  pure Stats
    { trainCount = 0
    , testCount  = 0
    , ..
    }

trainAndValid :: Model -> FilePath -> FilePath -> FilePath -> Int -> IO ()
trainAndValid m trainFile validFile statFile iters = do
  startedAt <- getEpochTime
  timerH <- newTVarIO startedAt
  proccedH <- newTVarIO 0
  trainCount <- countSample trainFile
  testCount <- countSample validFile

  replicateM_ iters $ do
    stats <- trainAndValidOne m trainFile validFile
              (resetProcced proccedH)
              (showStats startedAt timerH proccedH trainCount "Train" Nothing)
              (showStats startedAt timerH proccedH testCount "Test")
    saveStatsToFile statFile stats
      { trainCount = trainCount
      , testCount  = testCount
      }

prettyProc :: Int -> Int -> String
prettyProc total proc = show (fromIntegral (floor (prec * 10000)) / 100) ++ "%"
  where prec = fromIntegral proc / fromIntegral total


showStats :: Int64 -> TVar Int64 -> TVar Int -> Int -> String -> Maybe (TVar Int) -> Int64 -> Bool -> IO ()
showStats startedAt timerH proccedH total name mValidH currentStartedAt force = do
  unless force $ atomically $ modifyTVar' proccedH (+1)
  proc <- readTVarIO proccedH
  when (proc `mod` 1024 == 0 || force) $ do
    timer <- readTVarIO timerH
    now <- getEpochTime
    when ((now - timer) > 60 || force) $ do
      atomically $ writeTVar timerH now
      putStrLn $ name ++ " iters " ++ show proc ++ "/" ++ show total ++ " " ++ prettyProc total proc
      case mValidH of
        Nothing    -> pure ()
        Just validH -> do
          valid   <- readTVarIO validH
          putStrLn $ name ++ " score " ++ prettyProc proc valid


      let procced = now - currentStartedAt
          totalSpent = floor (fromIntegral procced / fromIntegral proc * fromIntegral total)

      putStrLn $ name ++ " Spent " ++ prettyTime procced
      putStrLn $ name ++ " will be finished in " ++ prettyTime (totalSpent - procced)
      putStrLn $ "Total Spent " ++ prettyTime (now - startedAt)


loadLabels :: FilePath -> TVar [Text] -> IO ()
loadLabels labelFile labelHandle =
  whenExists labelFile $ do
    labels <- T.split (=='\n') . T.strip <$> T.readFile labelFile
    atomically $ writeTVar labelHandle labels


saveLabels :: FilePath -> TVar [Text] -> IO ()
saveLabels labelFile labelHandle = do
  labels <- readTVarIO labelHandle
  T.writeFile labelFile1 $ T.intercalate "\n" labels
  renameFile labelFile1 labelFile

  where labelFile1 = labelFile ++ ".1"
