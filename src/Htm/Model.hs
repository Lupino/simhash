{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.Model
  ( train
  , test
  , Model (..)
  , loadModel
  , saveModel
  ) where


import           Control.Monad    (unless, when)
import           Data.Int         (Int64)
import           Data.List        (elemIndex)
import           Data.Text        (Text)
import qualified Data.Text        as T (intercalate, split, strip)
import qualified Data.Text.IO     as T (readFile, writeFile)
import           Htm.SimHash
import           Htm.Stats
import           Htm.Utils
import           Metro.Utils      (getEpochTime)
import           System.Directory (doesFileExist, renameFile)
import           UnliftIO         (TVar, atomically, modifyTVar', newTVarIO,
                                   readTVar, readTVarIO, writeTVar)

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

train :: Model -> Stats -> FilePath -> IO Stats
train shm@Model {..} stats dataFile = do
  countH <- newTVarIO 0
  startedAt <- getEpochTime
  readLineAndDo dataFile $ \str label -> do
    idx <- getLabelIdx labelHandle label
    learn str idx simhash
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


test :: Model -> Stats -> FilePath -> IO Stats
test Model {..} stats testFile = do
  totalH <- newTVarIO 0
  rightH <- newTVarIO 0
  labels <- readTVarIO labelHandle
  let size = length labels
  startedAt <- getEpochTime

  readLineAndDo testFile $ \str label -> do
    infers <- infer str size simhash
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
