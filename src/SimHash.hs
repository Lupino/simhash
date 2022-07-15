{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module SimHash
  ( train
  , test
  , SimHashModel
  , loadModel

  , RunnerQueue
  , newRunnerQueue
  , startInferRunner
  , simHashTask
  ) where


import           Control.Exception     (mask_)
import           Control.Monad         (forever, unless, void, when)
import           Control.Monad.Cont    (callCC, lift, runContT)
import           Data.Aeson            (encode)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Lazy  (toStrict)
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
withSimHash (SimHash fptr) f = withForeignPtr fptr f


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

  return SimHashModel {..}


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


train :: SimHashModel -> FilePath -> IO ()
train SimHashModel {..} dataFile = do
  readLineAndDo dataFile $ \label str -> do
    idx <- atomically $ do
      labels <- readTVar modelLabels
      case elemIndex label labels of
        Just idx -> pure idx
        Nothing -> do
          writeTVar modelLabels $! labels ++ [label]
          pure $ length labels

    learn model (encodeUtf8 str) idx

  saveToFile model . encodeUtf8 $ T.pack modelFile
  labels <- readTVarIO modelLabels
  T.writeFile (modelFile ++ ".labels") $ T.intercalate "\n" labels


argmax :: [Double] -> Int
argmax = go 0 0 0.0
  where go :: Int -> Int -> Double -> [Double] -> Int
        go _ idx _ [] = idx
        go c idx v (x:xs)
          | v > x = go (c + 1) idx v xs
          | otherwise = go (c + 1) c x xs


test :: SimHashModel -> FilePath -> IO Double
test SimHashModel {..} testFile = do
  totalH <- newTVarIO 0
  rightH <- newTVarIO 0
  labels <- readTVarIO modelLabels
  let size = length labels

  readLineAndDo testFile $ \label str -> do
    infers <- infer model (encodeUtf8 str) size
    atomically $ do
      modifyTVar' totalH (+1)
      case elemIndex label labels of
        Nothing -> pure ()
        Just idx ->
          when (argmax infers == idx) $ modifyTVar' rightH (+1)

  right <- readTVarIO rightH
  total <- readTVarIO totalH
  pure $ right / total


data QueueItem = QueueItem
  { itemMsg :: ByteString
  , itemRet :: TMVar [(Text, Double)]
  }


type RunnerQueue = TQueue QueueItem

newRunnerQueue :: IO RunnerQueue
newRunnerQueue = newTQueueIO


data InferRunner = InferRunner
  { runnerModel :: SimHashModel
  , runnerQueue :: RunnerQueue
  }


newInferRunner :: RunnerQueue -> FilePath -> IO InferRunner
newInferRunner runnerQueue path = do
  runnerModel <- loadModel path
  return InferRunner {..}


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


startInferRunner :: RunnerQueue -> FilePath -> IO (Async ())
startInferRunner queue path = do
  runner <- newInferRunner queue path
  async $ forever $ runInferRunner runner


simHashTask :: RunnerQueue -> JobM ()
simHashTask queue = do
  itemMsg <- workload
  itemRet <- newEmptyTMVarIO
  atomically $ writeTQueue queue QueueItem {..}
  ret <- atomically $ takeTMVar itemRet
  workDone_ $ toStrict $ encode ret
