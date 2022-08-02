{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.Model
  ( Model (..)
  , infer
  , trainAndValid

  , loadLabels
  , saveLabels
  ) where


import           Control.Monad    (when)
import           Data.ByteString  (ByteString)
import           Data.Int         (Int64)
import           Data.List        (sortBy)
import           Data.Text        (Text)
import qualified Data.Text        as T (intercalate, split, strip)
import qualified Data.Text.IO     as T (readFile, writeFile)
import           Htm.Stats        (Stats (..))
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



train :: Model -> FilePath -> Int64 -> TVar Int64 -> TVar Int -> IO ()
train Model {..} dataFile startedAt timerH totalH = do
  total <- countSample dataFile
  readLineAndDo dataFile $ \str label -> do
    atomically $ modifyTVar' totalH (+1)
    modelLearn str label
    showStats "Train" startedAt startedAt timerH totalH total False Nothing

  modelSave
  showStats "Train" startedAt startedAt timerH totalH total True Nothing


test :: Model -> FilePath -> Int64 -> Int64 -> TVar Int64 -> TVar Int -> TVar Int -> IO ()
test Model {..} testFile startedAt testStartedAt timerH totalH validH = do
  size <- length <$> readTVarIO modelLabels
  total <- countSample testFile

  readLineAndDo testFile $ \str label -> do
    atomically $ modifyTVar' totalH (+1)
    infers <- modelInfer str size
    idx <- getLabelIdx modelLabels label

    when (argmax infers == idx) $ atomically $ modifyTVar' validH (+1)
    showStats "Test" startedAt testStartedAt timerH totalH total False $ Just showScore

  showStats "Test" startedAt testStartedAt timerH totalH total True $ Just showScore

  where showScore :: IO ()
        showScore = do
          valid <- readTVarIO validH
          total <- readTVarIO totalH
          putStrLn $ "Test score " ++ prettyProc total valid


trainAndValid :: Model -> FilePath -> FilePath -> IO Stats
trainAndValid model@Model {..} trainFile validFile = do
  trainStartedAt <- getEpochTime
  timerH <- newTVarIO trainStartedAt
  totalH <- newTVarIO 0
  train model trainFile trainStartedAt timerH totalH

  testStartedAt <- getEpochTime
  testTotalH <- newTVarIO 0
  testValidH <- newTVarIO 0
  test model validFile trainStartedAt testStartedAt timerH testTotalH testValidH

  trainCount <- readTVarIO totalH
  testCount  <- readTVarIO testTotalH
  validCount <- readTVarIO testValidH

  testFinishedAt <- getEpochTime
  pure Stats {..}


prettyProc :: Int -> Int -> String
prettyProc total proc = show (fromIntegral (floor (prec * 10000)) / 100) ++ "%"
  where prec = fromIntegral proc / fromIntegral total


showStats :: String -> Int64 -> Int64 -> TVar Int64 -> TVar Int -> Int -> Bool -> Maybe (IO ()) -> IO ()
showStats name startedAt currentStartedAt timerH procH total force mEvent = do
  proc <- readTVarIO procH
  when (proc `mod` 1024 == 0 || force) $ do
    timer <- readTVarIO timerH
    now <- getEpochTime
    when ((now - timer) > 60 || force) $ do
      atomically $ writeTVar timerH now
      putStrLn $ name ++ " iters " ++ show proc ++ "/" ++ show total ++ " " ++ prettyProc total proc
      case mEvent of
        Nothing    -> pure ()
        Just event -> event

      putStrLn $ name ++ " Spent " ++ prettyTime (now - currentStartedAt)
      putStrLn $ name ++ " Finished in " ++ prettyTime (floor (fromIntegral (now - currentStartedAt) / fromIntegral proc * fromIntegral total))
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
