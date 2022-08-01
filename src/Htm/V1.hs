{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.V1
  ( loadModel
  ) where


import           Control.Monad    (unless, when)
import           Data.ByteString  (ByteString)
import           Data.Text        (Text)
import           Htm.Model        (Model (..), loadLabels, saveLabels)
import           Htm.SimHash      (SimHash, infer, learn, loadFromFile,
                                   loadFromFileV2, new, saveToFile, setup)
import           Htm.Utils        (getLabelIdx)
import           System.Directory (doesFileExist, renameFile)
import           UnliftIO         (TVar, newTVarIO)


loadModel :: FilePath -> IO Model
loadModel modelFile = do
  modelLabels <- newTVarIO []
  loadLabels labelFile modelLabels
  simhash <- new

  exists0 <- doesFileExist modelFile
  exists1 <- doesFileExist spFile
  unless (exists0 || exists1) $ setup simhash
  when exists0 $ loadFromFile modelFile simhash
  when exists1 $ loadFromFileV2 spFile clsrFile simhash

  pure Model
    { modelSave = saveModel modelFile simhash modelLabels
    , modelLearn = doLearn simhash modelLabels
    , modelInfer = doInfer simhash
    , ..
    }

  where spFile = modelFile ++ ".sp"
        clsrFile = modelFile ++ ".clsr"
        labelFile = modelFile ++ ".labels"


saveModel ::FilePath -> SimHash -> TVar [Text] -> IO ()
saveModel modelFile simhash labelHandle = do

  saveToFile (modelFile ++ ".1") simhash
  renameFile (modelFile ++ ".1") modelFile

  saveLabels (modelFile ++ ".labels") labelHandle


doLearn :: SimHash -> TVar [Text] -> ByteString -> Text -> IO ()
doLearn simhash labelHandle str label = do
  idx <- getLabelIdx labelHandle label
  learn str idx simhash


doInfer :: SimHash -> ByteString -> Int -> IO [Double]
doInfer simhash str size = infer str size simhash
