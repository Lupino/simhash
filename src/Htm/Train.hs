{-# LANGUAGE OverloadedStrings #-}

module Htm.Train
  ( train
  ) where


import           Control.Monad              (forever, replicateM_, when)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (intercalate, split, strip)
import qualified Data.Text.IO               as T (readFile, writeFile)
import           Htm.Classifier             (Classifier)
import qualified Htm.Classifier             as Clsr
import           Htm.Sdr                    (Sdr)
import qualified Htm.Sdr                    as Sdr
import           Htm.SimHashDocumentEncoder (SimHashDocumentEncoder,
                                             SimHashDocumentEncoderOpts (..))
import qualified Htm.SimHashDocumentEncoder as Encoder
import           Htm.SpatialPooler          (SpatialPooler)
import qualified Htm.SpatialPooler          as SP
import           Htm.Stats
import           Htm.Utils
import           Metro.Utils                (getEpochTime)
import           System.Directory           (doesFileExist, renameFile)
import           UnliftIO

startReadFile :: FilePath -> TQueue (TBQueue (ByteString, Text)) -> TVar Int -> IO ()
startReadFile fp queues totalH =
  readLineAndDo fp $ \str label -> do
    queue <- atomically $ readTQueue queues
    atomically $ writeTQueue queues queue
    atomically $ writeTBQueue queue (str, label)
    atomically $ modifyTVar' totalH (+1)

startEncoder
  :: MonadUnliftIO m
  => SimHashDocumentEncoder
  -> Int
  -> TBQueue (ByteString, Text)
  -> TQueue (Sdr, Text)
  -> m (Async ())
startEncoder encoder size iQueue oQueue = async $ forever $ do
  (str, label) <- atomically $ readTBQueue iQueue
  sdr <- liftIO $ Sdr.new size
  liftIO $ Encoder.encode str sdr encoder
  atomically $ writeTQueue oQueue (sdr, label)


startSP
  :: MonadUnliftIO m
  => SpatialPooler
  -> Bool -> Int
  -> TQueue (Sdr, Text) -> TQueue (Sdr, Text) -> m (Async ())
startSP sp learn size iQueue oQueue = async $ forever $ do
  (input, label) <- atomically $ readTQueue iQueue
  sdr <- liftIO $ Sdr.new size
  liftIO $ SP.compute input learn sdr sp
  atomically $ writeTQueue oQueue (sdr, label)


startClsrLearn
  :: MonadUnliftIO m
  => Classifier
  -> Int64 -> TVar Int -> TVar Int
  -> TVar [Text] -> TQueue (Sdr, Text) -> m (Async ())
startClsrLearn clsr startedAt totalH procH labels iQueue = async $ forever $ do
  (pattern, label) <- atomically $ readTQueue iQueue
  idx <- getLabelIdx labels label
  liftIO $ Clsr.learn pattern idx clsr
  atomically $ modifyTVar' procH (+1)
  liftIO $ showStats "Learn" startedAt totalH procH False Nothing


startClsrTest
  :: MonadUnliftIO m
  => Classifier
  -> Int64 -> TVar Int -> TVar Int -> TVar Int
  -> TVar [Text]
  -> TQueue (Sdr, Text) -> m (Async ())
startClsrTest clsr startedAt totalH procH rightH labelH iQueue = do
  size <- length <$> readTVarIO labelH
  async $ forever $ do
    (pattern, label) <- atomically $ readTQueue iQueue
    infers <- liftIO $ Clsr.infer pattern size clsr
    idx <- getLabelIdx labelH label
    atomically $ do
      modifyTVar' procH (+1)
      when (argmax infers == idx) $ modifyTVar' rightH (+1)

    liftIO $ showStats "Test" startedAt totalH procH False $ Just $ do
      right <- readTVarIO rightH
      total <- readTVarIO procH
      putStrLn $ "Test score " ++ show (fromIntegral right / fromIntegral total)


showStats :: String -> Int64 -> TVar Int -> TVar Int -> Bool -> Maybe (IO ()) -> IO ()
showStats name startedAt totalH procH force mEvent = do
  proc <- readTVarIO procH
  when (proc `mod` 2048 == 0 || force) $ do
    now <- getEpochTime
    when ((now - startedAt) `mod` 60 == 0 || force) $ do
      total <- readTVarIO totalH
      putStrLn $ name ++ " iters " ++ show proc ++ "/" ++ show total
      case mEvent of
        Nothing    -> pure ()
        Just event -> event

      putStrLn $ "Spent " ++ prettyTime (now - startedAt)


train :: FilePath -> FilePath -> FilePath -> IO ()
train modelFile trainFile validFile = do
  startedAt <- getEpochTime
  queues <- newTQueueIO
  encodedQueue <- newTQueueIO
  replicateM_ 2 $ do
    encoder <- Encoder.new SimHashDocumentEncoderOpts
      { optSize = 600
      , optSparsity = 0.2
      , optTokenSimilarity = True
      }
    queue <- newTBQueueIO 100
    atomically $ writeTQueue queues queue
    startEncoder encoder 600 queue encodedQueue

  sp <- SP.new 600 1600
  spFileExists <- doesFileExist spFile
  when spFileExists $ SP.loadFromFile sp $ fromString spFile

  spOutQueue <- newTQueueIO
  ioSp <- startSP sp True 1600 encodedQueue spOutQueue

  clsr <- Clsr.new
  clsrFileExists <- doesFileExist clsrFile
  when clsrFileExists $ Clsr.loadFromFile clsr $ fromString clsrFile

  labelH <- newTVarIO []
  labelFileExists <- doesFileExist labelFile
  when labelFileExists $ do
    labels <- T.split (=='\n') . T.strip <$> T.readFile (modelFile ++ ".labels")
    atomically $ writeTVar labelH labels


  totalH <- newTVarIO 0
  procH <- newTVarIO 0

  ioClsr <- startClsrLearn clsr startedAt totalH procH labelH spOutQueue
  startReadFile trainFile queues totalH

  total <- readTVarIO totalH
  atomically $ do
    proc <- readTVar procH
    if proc >= total then pure ()
                 else retrySTM

  showStats "Learn" startedAt totalH procH True Nothing

  cancel ioSp
  cancel ioClsr

  SP.saveToFile sp $ fromString spFile1
  Clsr.saveToFile clsr $ fromString clsrFile1
  labels <- readTVarIO labelH
  T.writeFile labelFile1 $ T.intercalate "\n" labels

  validStartedAt <- getEpochTime

  rightH <- newTVarIO 0
  ioSp1 <- startSP sp False 1600 encodedQueue spOutQueue

  totalValidH <- newTVarIO 0
  procValidH <- newTVarIO 0

  ioClsr1 <- startClsrTest clsr startedAt totalValidH procValidH rightH labelH spOutQueue
  startReadFile validFile queues totalValidH

  totalValid <- readTVarIO totalValidH
  atomically $ do
    proc <- readTVar procValidH
    if proc >= totalValid then pure ()
                 else retrySTM

  cancel ioSp1
  cancel ioClsr1

  right <- fromIntegral <$> readTVarIO rightH
  showStats "Test" startedAt totalValidH procValidH True $ Just $
    putStrLn $ "Test score " ++ show (right / fromIntegral totalValid)

  now <- getEpochTime

  saveStatsToFile statFile1 Stats
    { trainCount      = total
    , testCount       = totalValid
    , trainStartedAt  = startedAt
    , trainFinishedAt = validStartedAt
    , trainSpent      = prettyTime (validStartedAt - startedAt)
    , testStartedAt   = validStartedAt
    , testFinishedAt  = now
    , testSpent       = prettyTime (now - validStartedAt)
    , testScore       = floor (right * 10000 / fromIntegral totalValid)
    }

  renameFile statFile1 statFile
  renameFile spFile1 spFile
  renameFile clsrFile1 clsrFile
  renameFile labelFile1 labelFile

  where spFile = modelFile ++ ".sp"
        clsrFile = modelFile ++ ".clsr"
        labelFile = modelFile ++ ".labels"
        statFile = modelFile ++ ".stats.json"

        spFile1 = modelFile ++ ".sp.1"
        clsrFile1 = modelFile ++ ".clsr.1"
        labelFile1 = modelFile ++ ".labels.1"
        statFile1 = modelFile ++ ".stats.json.1"
