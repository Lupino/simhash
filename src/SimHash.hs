{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module SimHash
  ( train
  , test
  , SimHashModel
  , loadModel
  , Stats
  , emptyStats
  , saveStatsToFile

  , RunnerQueue
  , newRunnerQueue
  , startInferRunner
  , inferTask
  ) where


import           Control.Exception     (mask_)
import           Control.Monad         (forever, unless, void, when)
import           Control.Monad.Cont    (callCC, lift, runContT)
import           Data.Aeson            (ToJSON (..), encode, object, (.=))
import           Data.ByteString       (ByteString)
import           Data.ByteString.Lazy  (toStrict)
import qualified Data.ByteString.Lazy  as LB (writeFile)
import           Data.Int              (Int64)
import           Data.List             (elemIndex, sortBy)
import           Data.Text             (Text)
import qualified Data.Text             as T (drop, intercalate, length, null,
                                             pack, split, strip, takeWhile)
import           Data.Text.Encoding    (encodeUtf8)
import qualified Data.Text.IO          as T (hGetLine, readFile, writeFile)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Ptr           (FunPtr, Ptr)
import qualified Language.C.Inline.Cpp as C
import           Metro.Utils           (getEpochTime)
import           Periodic.Job          (JobM, workDone_, workload)
import           System.Directory      (doesFileExist)
import           System.IO             (IOMode (ReadMode), hClose, hIsEOF,
                                        openFile)
import           UnliftIO              (Async, TMVar, TQueue, TVar, async,
                                        atomically, modifyTVar',
                                        newEmptyTMVarIO, newTQueueIO, newTVarIO,
                                        readTQueue, readTVar, readTVarIO,
                                        takeTMVar, tryPutTMVar, writeTQueue,
                                        writeTVar)

data CSimHash

C.context (C.cppCtx <> C.bsCtx <> C.cppTypePairs [("simhash::SimHash", [t|CSimHash|])])
C.include "<SimHash.hpp>"

newCSimHash :: IO (Ptr CSimHash)
newCSimHash =
  [C.block| simhash::SimHash* {
    return new simhash::SimHash();
  }|]


cDeleteSimHash :: FunPtr (Ptr CSimHash -> IO ())
cDeleteSimHash =
  [C.funPtr|void deleteSimHash(simhash::SimHash* sh){delete sh;}|]


cSimHashSetup :: Ptr CSimHash -> IO ()
cSimHashSetup ptr =
  [C.exp| void {$(simhash::SimHash* ptr)->setup()}|]


cSimHashLearn :: Ptr CSimHash -> ByteString -> C.CInt -> IO ()
cSimHashLearn ptr str idx =
  [C.block| void {
    std::string str($bs-ptr:str);
    str.resize($bs-len:str);
    $(simhash::SimHash* ptr)->learn(str, $(int idx));
  }|]


cSimHashInfer :: Ptr CSimHash -> ByteString -> Ptr C.CDouble -> IO ()
cSimHashInfer ptr str out = do
  [C.block| void {
    std::string str($bs-ptr:str);
    str.resize($bs-len:str);
    $(simhash::SimHash* ptr)->infer(str, $(double* out));
  }|]


cSimHashAddMetrics :: Ptr CSimHash -> IO ()
cSimHashAddMetrics ptr =
  [C.exp| void {$(simhash::SimHash* ptr)->addMetrics()}|]


cSimHashShowMetrics :: Ptr CSimHash -> IO ()
cSimHashShowMetrics ptr =
  [C.exp| void {$(simhash::SimHash* ptr)->showMetrics()}|]


cSimHashSaveToFile :: Ptr CSimHash -> ByteString -> IO ()
cSimHashSaveToFile ptr fn = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(simhash::SimHash* ptr)->saveToFile(fn);
  }|]


cSimHashLoadFromFile :: Ptr CSimHash -> ByteString -> IO ()
cSimHashLoadFromFile ptr fn = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(simhash::SimHash* ptr)->loadFromFile(fn);
  }|]


newtype SimHash = SimHash (ForeignPtr CSimHash)

new :: IO SimHash
new = mask_ $ do
  ptr <- newCSimHash
  SimHash <$> newForeignPtr cDeleteSimHash ptr


setup :: SimHash -> IO ()
setup sh = withSimHash sh cSimHashSetup


withSimHash :: SimHash -> (Ptr CSimHash -> IO a) -> IO a
withSimHash (SimHash fptr) = withForeignPtr fptr


learn :: SimHash -> ByteString -> Int -> IO ()
learn sh str idx =
  withSimHash sh $ \ptr ->
    cSimHashLearn ptr str (fromIntegral idx)


infer :: SimHash -> ByteString -> Int -> IO [Double]
infer sh str size =
  withSimHash sh $ \ptr ->
    allocaArray size $ \out -> do
      cSimHashInfer ptr str out
      map realToFrac <$> peekArray size out


addMetrics :: SimHash -> IO ()
addMetrics sh = withSimHash sh cSimHashAddMetrics


showMetrics :: SimHash -> IO ()
showMetrics sh = withSimHash sh cSimHashShowMetrics


saveToFile :: SimHash -> ByteString -> IO ()
saveToFile sh fn = withSimHash sh $ flip cSimHashSaveToFile fn


loadFromFile :: SimHash -> ByteString -> IO ()
loadFromFile sh fn = withSimHash sh $ flip cSimHashLoadFromFile fn


data SimHashModel = SimHashModel
  { modelLabels :: TVar [Text]
  , modelFile   :: FilePath
  , model       :: SimHash
  }


loadModel :: FilePath -> IO SimHashModel
loadModel modelFile = do
  model <- new
  exists <- doesFileExist modelFile
  unless exists $ setup model
  modelLabels <- newTVarIO []

  when exists $ do
    loadFromFile model . encodeUtf8 $ T.pack modelFile
    labels <- T.split (=='\n') . T.strip <$> T.readFile (modelFile ++ ".labels")
    atomically $ writeTVar modelLabels labels

  pure SimHashModel {..}


readLineAndDo :: FilePath -> (Text -> Text -> IO ()) -> IO ()
readLineAndDo path f = do
  h <- openFile path ReadMode
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    eof <- lift $ hIsEOF h
    when eof $ exit ()
    line <- lift $ T.strip <$> T.hGetLine h
    let label = T.strip $ T.takeWhile (/=',') line
        str   = T.strip $ T.drop (T.length label + 1) line
    unless (T.null label || T.null str) $ do
      lift $ f label str

  hClose h


prettyTime :: Int64 -> String
prettyTime t0
  | h > 0 = show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"
  | m > 0 = show m ++ "m " ++ show s ++ "s"
  | otherwise = show s ++ "s"
  where s = t0 `mod` 60
        t1 = floor $ fromIntegral t0 / 60
        m = t1 `mod` 60
        h = floor $ fromIntegral t1 / 60


data Stats = Stats
  { trainCount      :: Int
  , testCount       :: Int
  , trainStartedAt  :: Int64
  , trainFinishedAt :: Int64
  , trainSpent      :: String
  , testStartedAt   :: Int64
  , testFinishedAt  :: Int64
  , testSpent       :: String
  , testScore       :: Int
  }
  deriving (Show)


emptyStats :: Stats
emptyStats = Stats
  { trainCount      = 0
  , testCount       = 0
  , trainStartedAt  = 0
  , trainFinishedAt = 0
  , trainSpent      = ""
  , testStartedAt   = 0
  , testFinishedAt  = 0
  , testSpent       = ""
  , testScore       = 0
  }


instance ToJSON Stats where
  toJSON Stats {..} = object
    [ "train_count"       .= trainCount
    , "test_count"        .= testCount
    , "started_at"        .= trainStartedAt
    , "train_started_at"  .= trainStartedAt
    , "train_iter"        .= trainCount
    , "train_finished_at" .= trainFinishedAt
    , "test_started_at"   .= testStartedAt
    , "test_iter"         .= testCount
    , "test_finished_at"  .= testFinishedAt
    , "score"             .= testScore
    , "finished_at"       .= testFinishedAt
    ]


saveStatsToFile :: FilePath -> Stats -> IO ()
saveStatsToFile path = LB.writeFile path . encode


train :: SimHashModel -> Stats -> FilePath -> IO Stats
train SimHashModel {..} stats dataFile = do
  countH <- newTVarIO 0
  startedAt <- getEpochTime
  readLineAndDo dataFile $ \label str -> do
    idx <- atomically $ do
      labels <- readTVar modelLabels
      case elemIndex label labels of
        Just idx -> pure idx
        Nothing -> do
          writeTVar modelLabels $! labels ++ [label]
          pure $ length labels

    learn model (encodeUtf8 str) idx
    addMetrics model
    count <- atomically $ do
      c <- readTVar countH
      writeTVar countH $! c + 1
      pure c

    when (count `mod` 1024 == 0) $ showTrainStats startedAt count

  count <- readTVarIO countH
  showTrainStats startedAt count
  finishedAt <- getEpochTime

  saveToFile model . encodeUtf8 $ T.pack modelFile
  labels <- readTVarIO modelLabels
  T.writeFile (modelFile ++ ".labels") $ T.intercalate "\n" labels

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
          showMetrics model


argmax :: [Double] -> Int
argmax = go 0 0 0.0
  where go :: Int -> Int -> Double -> [Double] -> Int
        go _ idx _ [] = idx
        go c idx v (x:xs)
          | v > x = go (c + 1) idx v xs
          | otherwise = go (c + 1) c x xs


test :: SimHashModel -> Stats -> FilePath -> IO Stats
test SimHashModel {..} stats testFile = do
  totalH <- newTVarIO 0
  rightH <- newTVarIO 0
  labels <- readTVarIO modelLabels
  let size = length labels
  startedAt <- getEpochTime

  readLineAndDo testFile $ \label str -> do
    infers <- infer model (encodeUtf8 str) size
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
  { itemMsg :: ByteString
  , itemRet :: TMVar [(Text, Double)]
  }


type RunnerQueue = TQueue QueueItem

newRunnerQueue :: IO (TQueue RunnerQueue)
newRunnerQueue = newTQueueIO


data InferRunner = InferRunner
  { runnerModel :: SimHashModel
  , runnerQueue :: RunnerQueue
  }


newInferRunner :: RunnerQueue -> FilePath -> IO InferRunner
newInferRunner runnerQueue path = do
  runnerModel <- loadModel path
  pure InferRunner {..}


runInferRunner :: InferRunner -> IO ()
runInferRunner InferRunner {..} = do
  QueueItem {..} <- atomically $ readTQueue runnerQueue
  labels <- readTVarIO $ modelLabels runnerModel
  let size = length labels
  infers <- infer (model runnerModel) itemMsg size
  atomically
    . void
    . tryPutTMVar itemRet
    $! take 10
    . sortBy (\(_, a) (_, b) -> compare b a)
    $ zip labels infers


startInferRunner :: TQueue RunnerQueue -> FilePath -> IO (Async ())
startInferRunner tqueue path = do
  queue <- newTQueueIO
  atomically $ writeTQueue tqueue queue
  runner <- newInferRunner queue path
  async $ forever $ runInferRunner runner


inferTask :: TQueue RunnerQueue -> JobM ()
inferTask tqueue = do
  itemMsg <- workload
  itemRet <- newEmptyTMVarIO
  queue <- atomically $ readTQueue tqueue
  atomically $ writeTQueue queue QueueItem {..}
  atomically $ writeTQueue tqueue queue
  ret <- atomically $ takeTMVar itemRet
  workDone_ $ toStrict $ encode ret
