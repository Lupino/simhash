{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Monad       (void)
import           Data.String         (fromString)
import           Options.Applicative
import           Periodic.Worker     (addFunc, startWorkerM, work)
import           SimHash             (inferLearnTask, newRunnerQueue,
                                      readRunnerQueue, startRunner, startSaver)

data Options = Options
  { modelName  :: String
  , modelFile  :: FilePath
  , host       :: String
  , threadSize :: Int
  }


parser :: Parser Options
parser =
  Options
    <$> strOption
      ( long "name"
      <> short 'n'
      <> metavar "NAME"
      <> help "SimHash model name"
      <> value "simhash")
    <*> strOption
      ( long "file"
      <> short 'f'
      <> metavar "FILE"
      <> help "SimHash model file"
      <> value "simhash.model")
    <*> strOption
      ( long "host"
      <> short 'H'
      <> metavar "HOST"
      <> help "Periodic Server PORT"
      <> value "unix:///tmp/periodic.sock")
    <*> option auto
      ( long "thread"
      <> short 't'
      <> metavar "THREAD"
      <> help "Worker thread size"
      <> value 20)


main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "SimHash Infer And Learn Runner"
     <> header "simhash-infer-learn - SimHash Infer And Learn Runner" )

program :: Options -> IO ()
program Options{..} = do
  queue <- newRunnerQueue
  (runner, _) <- startRunner queue modelFile
  void $ startSaver runner
  rq <- readRunnerQueue queue
  startWorkerM host $ do
    addFunc (fromString modelName) $ inferLearnTask rq
    work threadSize
