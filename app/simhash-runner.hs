{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where


import           Control.Monad              (replicateM_, void)
import           Data.String                (fromString)
import           Htm.Model                  (trainAndValid)
import           Htm.Runner                 (inferOne, newQueue, startRunner,
                                             startSaver)
import qualified Htm.V1                     as V1
import qualified Htm.V2                     as V2
import           Options.Applicative
import           Options.Applicative.Arrows
import           Periodic.Worker            (addFunc, startWorkerM, work)
import           SimHash                    (inferLearnTask, inferTask,
                                             newQueues, newRQueue)

data Args = Args CommonOpts Command
  deriving Show

newtype CommonOpts = CommonOpts
  { optModelFile :: String }
  deriving Show

data Command
  = Train FilePath FilePath Int
  | Test String
  | Infer PeriodicOpts Int
  | InferLearn PeriodicOpts
  | TrainV2 FilePath FilePath Int
  | TestV2 String
  | InferV2 PeriodicOpts Int
  | InferLearnV2 PeriodicOpts
  deriving Show


data PeriodicOpts = PeriodicOpts
  { optFuncName :: String
  , optHost     :: String
  , optWorkSize :: Int
  }
  deriving Show


commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$>  strOption
      ( long "file"
      <> short 'f'
      <> metavar "FILE"
      <> help "SimHash model file"
      <> value "simhash.model")


periodicOpts :: Parser PeriodicOpts
periodicOpts = PeriodicOpts
  <$> strOption
    ( long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "FuncName"
    <> value "simhash")
  <*> strOption
    ( long "host"
    <> short 'H'
    <> metavar "HOST"
    <> help "Periodic Server PORT"
    <> value "unix:///tmp/periodic.sock")
  <*> option auto
    ( long "work-size"
    <> short 'w'
    <> metavar "WORK SIZE"
    <> value 20)

trainParser :: Parser Command
trainParser = Train
  <$> strOption
    ( long "data"
    <> short 'd'
    <> metavar "DATA FILE"
    <> value "data.txt")
  <*> strOption
    ( long "test"
    <> short 't'
    <> metavar "TEST FILE"
    <> value "test.txt")
  <*> option auto
    ( long "iters"
    <> metavar "ITERS"
    <> value 1)

trainV2Parser :: Parser Command
trainV2Parser = TrainV2
  <$> strOption
    ( long "data"
    <> short 'd'
    <> metavar "DATA FILE"
    <> value "data.txt")
  <*> strOption
    ( long "test"
    <> short 't'
    <> metavar "TEST FILE"
    <> value "test.txt")
  <*> option auto
    ( long "iters"
    <> metavar "ITERS"
    <> value 1)

testParser :: Parser Command
testParser = Test
  <$> strOption
    ( long "str"
    <> short 's'
    <> metavar "STRING"
    <> value "")

testV2Parser :: Parser Command
testV2Parser = TestV2
  <$> strOption
    ( long "str"
    <> short 's'
    <> metavar "STRING"
    <> value "")


inferParser :: Parser Command
inferParser = runA $ proc () -> do
  opts <- asA periodicOpts -< ()
  runnerSize <- (asA . option auto)
            ( long "runner-size"
           <> short 's'
           <> metavar "RUNNER SIZE"
           <> value 10 ) -< ()

  returnA -< Infer opts runnerSize


inferV2Parser :: Parser Command
inferV2Parser = runA $ proc () -> do
  opts <- asA periodicOpts -< ()
  runnerSize <- (asA . option auto)
            ( long "runner-size"
           <> short 's'
           <> metavar "RUNNER SIZE"
           <> value 10 ) -< ()

  returnA -< InferV2 opts runnerSize


inferLearnParser :: Parser Command
inferLearnParser = runA $ proc () -> do
  opts <- asA periodicOpts -< ()
  returnA -< InferLearn opts


inferLearnV2Parser :: Parser Command
inferLearnV2Parser = runA $ proc () -> do
  opts <- asA periodicOpts -< ()
  returnA -< InferLearnV2 opts


version :: Parser (a -> a)
version = infoOption "0.0.0"
  (  long "version"
  <> help "Print version information" )

parser :: Parser Args
parser = runA $ proc () -> do
  opts <- asA commonOpts -< ()
  cmds <- (asA . hsubparser)
            ( command "train"
              (info trainParser
                    (progDesc "Train simhash model"))
           <> command "test"
              (info testParser
                    (progDesc "Test a string"))
           <> command "infer"
              (info inferParser
                    (progDesc "Run infer task"))
           <> command "infer-learn"
              (info inferLearnParser
                    (progDesc "Run infer learn task"))
           <> command "v2-train"
              (info trainV2Parser
                    (progDesc "Train simhash model v2"))
           <> command "v2-test"
              (info testParser
                    (progDesc "Test a string v2"))
           <> command "v2-infer"
              (info inferParser
                    (progDesc "Run infer task v2"))
           <> command "v2-infer-learn"
              (info inferLearnParser
                    (progDesc "Run infer learn task v2"))
            ) -< ()
  A version >>> A helper -< Args opts cmds


program :: Args -> IO ()
program (Args CommonOpts{..} (Train trainFile validFile iters)) = do
  model <- V1.loadModel optModelFile
  trainAndValid model trainFile validFile (optModelFile ++ ".stats.json") iters
program (Args CommonOpts{..} (Test s)) = do
  queue <- newQueue
  void $ startRunner queue $ V1.loadModel optModelFile
  ret <- inferOne queue (fromString s)
  print ret
program (Args CommonOpts{..} (Infer PeriodicOpts {..} runnerSize)) = do
  queues <- newQueues
  replicateM_ runnerSize $ do
    queue <-  newRQueue queues
    void $ startRunner queue $ V1.loadModel optModelFile
  startWorkerM optHost $ do
    addFunc (fromString optFuncName) $ inferTask queues
    work optWorkSize
program (Args CommonOpts{..} (InferLearn PeriodicOpts {..})) = do
  queue <- newQueue
  (runner, _) <- startRunner queue $ V1.loadModel optModelFile
  void $ startSaver runner
  startWorkerM optHost $ do
    addFunc (fromString optFuncName) $ inferLearnTask queue
    work optWorkSize
program (Args CommonOpts{..} (TrainV2 trainFile validFile iters)) = do
  model <- V2.loadModel optModelFile
  trainAndValid model trainFile validFile (optModelFile ++ ".stats.json") iters
program (Args CommonOpts{..} (TestV2 s)) = do
  queue <- newQueue
  void $ startRunner queue $ V2.loadModel optModelFile
  ret <- inferOne queue (fromString s)
  print ret
program (Args CommonOpts{..} (InferV2 PeriodicOpts {..} runnerSize)) = do
  queues <- newQueues
  replicateM_ runnerSize $ do
    queue <-  newRQueue queues
    void $ startRunner queue $ V2.loadModel optModelFile
  startWorkerM optHost $ do
    addFunc (fromString optFuncName) $ inferTask queues
    work optWorkSize
program (Args CommonOpts{..} (InferLearnV2 PeriodicOpts {..})) = do
  queue <- newQueue
  (runner, _) <- startRunner queue $ V2.loadModel optModelFile
  void $ startSaver runner
  startWorkerM optHost $ do
    addFunc (fromString optFuncName) $ inferLearnTask queue
    work optWorkSize

pinfo :: ParserInfo Args
pinfo = info parser
  ( progDesc "SimHash Runner" )

main :: IO ()
main = customExecParser (prefs helpShowGlobals) pinfo >>= program
