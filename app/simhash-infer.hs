{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Monad       (replicateM_)
import           Data.String         (fromString)
import           Options.Applicative
import           Periodic.Worker     (addFunc, startWorkerM, work)
import           SimHash             (inferTask, newRunnerQueue,
                                      startInferRunner)

data Options = Options
  { modelName  :: String
  , modelFile  :: FilePath
  , runnerSize :: Int
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
    <*> option auto
      ( long "size"
      <> short 's'
      <> metavar "SIZE"
      <> help "SimHash Runner size"
      <> value 10)
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
     <> progDesc "SimHash Infer Runner"
     <> header "simhash-infer - SimHash Infer Runner" )

program :: Options -> IO ()
program Options{..} = do
  queue <- newRunnerQueue
  replicateM_ runnerSize $ startInferRunner queue modelFile
  startWorkerM host $ do
    addFunc (fromString modelName) $ inferTask queue
    work threadSize
