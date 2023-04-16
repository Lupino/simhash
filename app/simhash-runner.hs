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
  = TrainV2 FilePath FilePath FilePath Int
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

trainV2Parser :: Parser Command
trainV2Parser = TrainV2
  <$> strOption
    ( long "boot"
    <> short 'b'
    <> metavar "BOOT FILE"
    <> value "")
  <*> strOption
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

testV2Parser :: Parser Command
testV2Parser = TestV2
  <$> strOption
    ( long "str"
    <> short 's'
    <> metavar "STRING"
    <> value "")

inferV2Parser :: Parser Command
inferV2Parser = runA $ proc () -> do
  opts <- asA periodicOpts -< ()
  runnerSize <- (asA . option auto)
            ( long "runner-size"
           <> short 's'
           <> metavar "RUNNER SIZE"
           <> value 10 ) -< ()

  returnA -< InferV2 opts runnerSize

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
           (  command "v2-train"
              (info trainV2Parser
                    (progDesc "Train simhash model v2"))
           <> command "v2-test"
              (info testV2Parser
                    (progDesc "Test a string v2"))
           <> command "v2-infer"
              (info inferV2Parser
                    (progDesc "Run infer task v2"))
           <> command "v2-infer-learn"
              (info inferLearnV2Parser
                    (progDesc "Run infer learn task v2"))
            ) -< ()
  A version >>> A helper -< Args opts cmds


asMaybe :: String -> Maybe String
asMaybe "" = Nothing
asMaybe s  = Just s


program :: Args -> IO ()
program (Args CommonOpts{..} (TrainV2 bootFile trainFile validFile iters)) = do

  trainAndValid
    (V2.loadModel (asMaybe bootFile) optModelFile) trainFile validFile
    (optModelFile ++ ".stats.json") iters
program (Args CommonOpts{..} (TestV2 s)) = do
  queue <- newQueue
  void $ startRunner queue $ V2.loadModel Nothing optModelFile
  ret <- inferOne queue (fromString s)
  print ret
program (Args CommonOpts{..} (InferV2 PeriodicOpts {..} runnerSize)) = do
  queues <- newQueues
  replicateM_ runnerSize $ do
    queue <-  newRQueue queues
    void $ startRunner queue $ V2.loadModel Nothing optModelFile
  startWorkerM optHost $ do
    void $ addFunc (fromString optFuncName) $ inferTask queues
    work optWorkSize
program (Args CommonOpts{..} (InferLearnV2 PeriodicOpts {..})) = do
  queue <- newQueue
  (runner, _) <- startRunner queue $ V2.loadModel Nothing optModelFile
  void $ startSaver runner
  startWorkerM optHost $ do
    void $ addFunc (fromString optFuncName) $ inferLearnTask queue
    work optWorkSize

pinfo :: ParserInfo Args
pinfo = info parser
  ( progDesc "SimHash Runner" )

main :: IO ()
main = customExecParser (prefs helpShowGlobals) pinfo >>= program
