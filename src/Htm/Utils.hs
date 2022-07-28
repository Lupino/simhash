module Htm.Utils
  ( getLabelIdx
  , readLineAndDo
  , argmax
  , prettyTime
  ) where


import           Control.Monad      (forever, unless, when)
import           Control.Monad.Cont (callCC, lift, runContT)
import           Data.ByteString    (ByteString)
import           Data.Int           (Int64)
import           Data.List          (elemIndex)
import           Data.Text          (Text)
import qualified Data.Text          as T (drop, length, null, strip, takeWhile)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO       as T (hGetLine)
import           UnliftIO


getLabelIdx :: MonadIO m => TVar [Text] -> Text -> m Int
getLabelIdx h label = atomically $ do
  labels <- readTVar h
  case elemIndex label labels of
    Just idx -> pure idx
    Nothing -> do
      writeTVar h $! labels ++ [label]
      pure $ length labels


splitLabelAndMsg :: Text -> (Text, Text)
splitLabelAndMsg msg = (label, str)
  where label = T.strip $ T.takeWhile (/=',') msg
        str   = T.strip $ T.drop (T.length label + 1) msg


readLineAndDo :: FilePath -> (ByteString -> Text -> IO ()) -> IO ()
readLineAndDo path f = do
  h <- openFile path ReadMode
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    eof <- lift $ hIsEOF h
    when eof $ exit ()
    (label, str) <- lift $ splitLabelAndMsg <$> T.hGetLine h
    unless (T.null label || T.null str) $
      lift $ f (encodeUtf8 str) label

  hClose h


argmax :: [Double] -> Int
argmax = go 0 0 0.0
  where go :: Int -> Int -> Double -> [Double] -> Int
        go _ idx _ [] = idx
        go c idx v (x:xs)
          | v > x = go (c + 1) idx v xs
          | otherwise = go (c + 1) c x xs


prettyTime :: Int64 -> String
prettyTime t0
  | h > 0 = show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"
  | m > 0 = show m ++ "m " ++ show s ++ "s"
  | otherwise = show s ++ "s"
  where s = t0 `mod` 60
        t1 = floor $ fromIntegral t0 / 60
        m = t1 `mod` 60
        h = floor $ fromIntegral t1 / 60
