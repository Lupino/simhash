{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.Model
  ( Model (..)
  , loadModel
  , saveModel
  , trainAndValid
  ) where


import           Control.Monad    (unless, when)
import           Data.Int         (Int64)
import           Data.Text        (Text)
import qualified Data.Text        as T (intercalate, split, strip)
import qualified Data.Text.IO     as T (readFile, writeFile)
import           Htm.SimHash
import           Htm.Stats
import           Htm.Utils
import           Metro.Utils      (getEpochTime)
import           System.Directory (doesFileExist, renameFile)
import           UnliftIO         (TVar, atomically, modifyTVar', newTVarIO,
                                   readTVarIO, writeTVar)

data Model = Model
  { labelHandle :: TVar [Text]
  , modelFile   :: FilePath
  , simhash     :: SimHash
  }


loadModel :: FilePath -> IO Model
loadModel modelFile = do
  labelHandle <- newTVarIO []
  simhash <- new

  exists0 <- doesFileExist modelFile
  exists1 <- doesFileExist spFile
  unless (exists0 || exists1) $ setup simhash

  when (exists0 || exists1) $ do
    labels <- T.split (=='\n') . T.strip <$> T.readFile labelFile
    atomically $ writeTVar labelHandle labels

  when exists0 $ loadFromFile modelFile simhash
  when exists1 $ loadFromFileV2 spFile clsrFile simhash

  pure Model {..}

  where spFile = modelFile ++ ".sp"
        clsrFile = modelFile ++ ".clsr"
        labelFile = modelFile ++ ".labels"


saveModel :: Model -> IO ()
saveModel Model {..} = do
  saveToFile (modelFile ++ ".1") simhash
  labels <- readTVarIO labelHandle
  T.writeFile (modelFile ++ ".labels.1") $ T.intercalate "\n" labels
  renameFile (modelFile ++ ".1") modelFile
  renameFile (modelFile ++ ".labels.1") (modelFile ++ ".labels")


train :: Model -> FilePath -> Int64 -> TVar Int64 -> TVar Int -> IO ()
train shm@Model {..} dataFile startedAt timerH totalH = do
  readLineAndDo dataFile $ \str label -> do
    idx <- getLabelIdx labelHandle label
    learn str idx simhash
    atomically $ modifyTVar' totalH (+1)
    showStats "Train" startedAt timerH totalH False Nothing

  saveModel shm
  showStats "Train" startedAt timerH totalH True Nothing


test :: Model -> FilePath -> Int64 -> TVar Int64 -> TVar Int -> TVar Int -> IO ()
test Model {..} testFile startedAt timerH totalH rightH = do
  size <- length <$> readTVarIO labelHandle

  readLineAndDo testFile $ \str label -> do
    infers <- infer str size simhash
    idx <- getLabelIdx labelHandle label
    atomically $ do
      modifyTVar' totalH (+1)
      when (argmax infers == idx) $ modifyTVar' rightH (+1)
    showStats "Test" startedAt timerH totalH False $ Just $ showScore

  showStats "Test" startedAt timerH totalH True $ Just showScore

  where showScore :: IO ()
        showScore = do
          right <- readTVarIO rightH
          total <- readTVarIO totalH
          putStrLn $ "Test score " ++ show (fromIntegral right / fromIntegral total)


trainAndValid :: FilePath -> FilePath -> FilePath -> IO ()
trainAndValid mFile trainFile validFile = do
  model <- loadModel mFile
  trainStartedAt <- getEpochTime
  timerH <- newTVarIO trainStartedAt
  totalH <- newTVarIO 0
  train model trainFile trainStartedAt timerH totalH

  testStartedAt <- getEpochTime
  testTotalH <- newTVarIO 0
  testRightH <- newTVarIO 0
  test model validFile trainStartedAt timerH totalH testRightH

  trainCount <- readTVarIO totalH
  testCount  <- readTVarIO testTotalH
  rightCount <- readTVarIO testRightH

  testFinishedAt <- getEpochTime
  saveStatsToFile statFile1 Stats
    { testScore       = floor (fromIntegral rightCount * 10000 / fromIntegral testCount)
    , ..
    }
  renameFile statFile1 statFile
  where statFile = mFile ++ ".stats.json"
        statFile1 = statFile ++ ".1"


showStats :: String -> Int64 -> TVar Int64 -> TVar Int -> Bool -> Maybe (IO ()) -> IO ()
showStats name startedAt timerH procH force mEvent = do
  proc <- readTVarIO procH
  when (proc `mod` 1024 == 0 || force) $ do
    timer <- readTVarIO timerH
    now <- getEpochTime
    when ((now - timer) > 60 || force) $ do
      atomically $ writeTVar timerH now
      putStrLn $ name ++ " iters " ++ show proc
      case mEvent of
        Nothing    -> pure ()
        Just event -> event

      putStrLn $ "Spent " ++ prettyTime (now - startedAt)
