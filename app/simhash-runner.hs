{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where


import           Control.Monad              (replicateM_, void)
import           Data.String                (fromString)
import           Options.Applicative
import           Options.Applicative.Arrows
import           Periodic.Worker            (addFunc, startWorkerM, work)
import           SimHash                    (emptyStats, inferLearnTask,
                                             inferOne, inferTask, loadModel,
                                             newRunnerQueue, readRunnerQueue,
                                             saveStatsToFile, startRunner,
                                             startSaver, test, train)

data Args = Args CommonOpts Command
  deriving Show

newtype CommonOpts = CommonOpts
  { optModelFile :: String }
  deriving Show

data Command
  = Train FilePath FilePath
  | Test String
  | Infer PeriodicOpts Int
  | InferLearn PeriodicOpts
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

testParser :: Parser Command
testParser = Test
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

inferLearnParser :: Parser Command
inferLearnParser = runA $ proc () -> do
  opts <- asA periodicOpts -< ()
  returnA -< InferLearn opts


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
                    (progDesc "Run infer learn task")) ) -< ()
  A version >>> A helper -< Args opts cmds


program :: Args -> IO ()
program (Args CommonOpts{..} (Train dataFile testFile)) = do
  model <- loadModel optModelFile
  stats0 <- train model emptyStats dataFile
  stats1 <- test model stats0 testFile
  saveStatsToFile (optModelFile ++ ".stats.json") stats1
program (Args CommonOpts{..} (Test s)) = do
  queues <- newRunnerQueue
  void $ startRunner queues optModelFile
  rq <- readRunnerQueue queues
  ret <- inferOne rq (fromString s)
  print ret
program (Args CommonOpts{..} (Infer PeriodicOpts {..} runnerSize)) = do
  queue <- newRunnerQueue
  replicateM_ runnerSize $ startRunner queue optModelFile
  startWorkerM optHost $ do
    addFunc (fromString optFuncName) $ inferTask queue
    work optWorkSize
program (Args CommonOpts{..} (InferLearn PeriodicOpts {..})) = do
  queue <- newRunnerQueue
  (runner, _) <- startRunner queue optModelFile
  void $ startSaver runner
  rq <- readRunnerQueue queue
  startWorkerM optHost $ do
    addFunc (fromString optFuncName) $ inferLearnTask rq
    work optWorkSize

pinfo :: ParserInfo Args
pinfo = info parser
  ( progDesc "SimHash Runner" )

main :: IO ()
main = customExecParser (prefs helpShowGlobals) pinfo >>= program
