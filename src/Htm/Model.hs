{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.Model
  ( Model (..)
  , infer
  , trainAndValid
  ) where


import           Control.Monad   (replicateM_, unless, when)
import           Data.ByteString (ByteString)
import           Data.Int        (Int64)
import           Data.List       (sortBy)
import           Data.Text       (Text)
import           Htm.Stats       (Stats (..), saveStatsToFile)
import           Htm.Utils       (argmax, getLabelIdx, prettyTime,
                                  readLineAndDo)
import           Metro.Utils     (getEpochTime)
import           UnliftIO        (TVar, atomically, modifyTVar', newTVarIO,
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

trainAndValid :: IO Model -> FilePath -> FilePath -> FilePath -> Int -> IO ()
trainAndValid io trainFile validFile statFile iters = do
  startedAt <- getEpochTime
  timerH <- newTVarIO startedAt
  proccedH <- newTVarIO 0
  sampleProcced <- newTVarIO 0
  trainCount <- countSample trainFile
  testCount <- countSample validFile
  iterH <- newTVarIO 0

  let totalSample = (trainCount + testCount) * iters
      showProc = showStats startedAt timerH iterH iters sampleProcced totalSample proccedH

  replicateM_ iters $ do
    m <- io
    atomically $ modifyTVar' iterH (+1)
    stats <- trainAndValidOne m trainFile validFile
              (resetProcced proccedH)
              (showProc trainCount "Train" Nothing)
              (showProc testCount "Test")
    saveStatsToFile statFile stats
      { trainCount = trainCount
      , testCount  = testCount
      }

prettyProc :: Int -> Int -> String
prettyProc total proc = show (fromIntegral (floor (prec * 10000)) / 100) ++ "%"
  where prec = fromIntegral proc / fromIntegral total


showStats
  :: Int64 -> TVar Int64
  -> TVar Int -> Int
  -> TVar Int -> Int
  -> TVar Int -> Int
  -> String
  -> Maybe (TVar Int) -> Int64 -> Bool -> IO ()
showStats
    startedAt timerH
    iterH iters
    sampleProcced totalSample
    proccedH total
    name mValidH currentStartedAt force = do
  unless force $ atomically $ do
    modifyTVar' proccedH (+1)
    modifyTVar' sampleProcced (+1)
  proc <- readTVarIO proccedH
  when (proc `mod` 1024 == 0 || force) $ do
    timer <- readTVarIO timerH
    now <- getEpochTime
    when ((now - timer) > 60 || force) $ do
      atomically $ writeTVar timerH now
      iter <- readTVarIO iterH

      sampleCount <- readTVarIO sampleProcced

      let iterLabel = " iter(" ++ minusStr iter iters ++ ") "
          proccedSpent = now - currentStartedAt
          totalSpent = calcSpent proccedSpent proc total

          sampleProccedSpent = now - startedAt
          sampleTotalSpent = calcSpent sampleProccedSpent sampleCount totalSample

      putStrLn $ name ++ iterLabel ++ minusStr proc total ++ " " ++ prettyProc total proc

      case mValidH of
        Nothing    -> pure ()
        Just validH -> do
          valid   <- readTVarIO validH
          putStrLn $ name ++ " score " ++ prettyProc proc valid

      putStrLn $ name ++ iterLabel ++ " Spent " ++ prettyTime proccedSpent
      putStrLn $ name ++ iterLabel ++ " will be finished in " ++ prettyTime (totalSpent - proccedSpent)
      putStrLn ""
      putStrLn $ "Total Spent " ++ prettyTime (now - startedAt)
      putStrLn $ "TrainAndValid will be finished in " ++ prettyTime (sampleTotalSpent - sampleProccedSpent)
      putStrLn ""

minusStr :: Int -> Int -> String
minusStr a b = show a ++ "/" ++ show b

calcSpent :: Int64 -> Int -> Int -> Int64
calcSpent procced proc total =
  floor (fromIntegral procced / fromIntegral proc * fromIntegral total)
