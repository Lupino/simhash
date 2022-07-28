{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.Stats
  ( Stats (..)
  , saveStatsToFile
  ) where


import           Data.Aeson           (ToJSON (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LB (writeFile)
import           Data.Int             (Int64)

data Stats = Stats
  { trainCount     :: Int
  , testCount      :: Int
  , trainStartedAt :: Int64
  , testStartedAt  :: Int64
  , testFinishedAt :: Int64
  , testScore      :: Int
  }
  deriving (Show)


instance ToJSON Stats where
  toJSON Stats {..} = object
    [ "train_count"       .= trainCount
    , "test_count"        .= testCount
    , "started_at"        .= trainStartedAt
    , "train_started_at"  .= trainStartedAt
    , "train_iter"        .= trainCount
    , "train_finished_at" .= testStartedAt
    , "test_started_at"   .= testStartedAt
    , "test_iter"         .= testCount
    , "test_finished_at"  .= testFinishedAt
    , "score"             .= testScore
    , "finished_at"       .= testFinishedAt
    ]


saveStatsToFile :: FilePath -> Stats -> IO ()
saveStatsToFile path = LB.writeFile path . encode
