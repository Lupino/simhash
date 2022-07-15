{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Options.Applicative
import           SimHash             (loadModel, test, train)

data Options = Options
  { dataFile  :: FilePath
  , modelFile :: FilePath
  , testFile  :: FilePath
  }


parser :: Parser Options
parser =
  Options
    <$> strOption
      ( long "data"
      <> short 'd'
      <> metavar "DATA FILE"
      <> help "Data file"
      <> value "data.txt")
    <*> strOption
      ( long "model"
      <> short 'm'
      <> metavar "MODEL FILE"
      <> help "SimHash model file"
      <> value "simhash.model")
    <*> strOption
      ( long "test"
      <> short 't'
      <> metavar "TEST FILE"
      <> help "test file"
      <> value "test.txt")


main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "SimHash Train and Vaild"
     <> header "simhash-train - SimHash Train and Vaild" )

program :: Options -> IO ()
program Options{..} = do
  model <- loadModel modelFile
  train model dataFile
  score <- test model testFile
  print score
