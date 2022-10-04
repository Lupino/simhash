{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.V2
  ( loadModel
  ) where


import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             (.!=), (.:), (.:?))
import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import           Data.Yaml                  (decodeFileEither)
import           Htm.Classifier             (Classifier)
import qualified Htm.Classifier             as Clsr
import           Htm.Model                  (Model (..), loadLabels, saveLabels)
import           Htm.Sdr                    (Sdr)
import qualified Htm.Sdr                    as Sdr
import           Htm.SimHashDocumentEncoder (SimHashDocumentEncoder,
                                             SimHashDocumentEncoderOpts (..))
import qualified Htm.SimHashDocumentEncoder as Encoder
import           Htm.SpatialPooler          (SpatialPooler)
import qualified Htm.SpatialPooler          as SP
import           Htm.Utils                  (getLabelIdx, whenExists)
import           System.Directory           (renameFile)
import           UnliftIO                   (TVar, newTVarIO)

data V2 = V2
  { labelHandle  :: TVar [Text]
  , modelFile    :: FilePath
  , modelEncoder :: SimHashDocumentEncoder
  , modelSp      :: SpatialPooler
  , modelClsr    :: Classifier
  , modelEncSdr  :: Sdr
  , modelSpSdr   :: Sdr
  }


data V2Opts = V2Opts
  { optEncoderOpts :: SimHashDocumentEncoderOpts
  , optColumnSize  :: Int
  }
  deriving Show

instance FromJSON V2Opts where
  parseJSON = withObject "V2Opts" $ \o -> do
    optEncoderOpts <- o .: "encoder"
    optColumnSize  <- o .:? "column_size" .!= 1600
    return V2Opts {..}


v2Opts :: V2Opts
v2Opts = V2Opts
  { optEncoderOpts = SimHashDocumentEncoderOpts
    { optSize = 600
    , optSparsity = 0.2
    , optTokenSimilarity = True
    }
  , optColumnSize = 1600
  }


loadV2 :: V2Opts -> FilePath -> IO V2
loadV2 V2Opts {..} modelFile = do
  labelHandle <- newTVarIO []
  modelEncoder <- Encoder.new optEncoderOpts
  modelSp <- SP.new (optSize optEncoderOpts) optColumnSize
  modelClsr <- Clsr.new
  modelEncSdr <- Sdr.new (optSize optEncoderOpts)
  modelSpSdr <- Sdr.new optColumnSize

  whenExists spFile $ SP.loadFromFile spFile modelSp
  whenExists clsrFile $ Clsr.loadFromFile clsrFile modelClsr
  loadLabels labelFile labelHandle

  pure V2 {..}

  where spFile = modelFile ++ ".sp"
        clsrFile = modelFile ++ ".clsr"
        labelFile = modelFile ++ ".labels"


saveV2 :: V2 -> IO ()
saveV2 V2 {..} = do
  SP.saveToFile spFile1 modelSp
  Clsr.saveToFile clsrFile1 modelClsr
  saveLabels labelFile labelHandle

  renameFile spFile1 spFile
  renameFile clsrFile1 clsrFile

  where spFile = modelFile ++ ".sp"
        clsrFile = modelFile ++ ".clsr"
        labelFile = modelFile ++ ".labels"

        spFile1 = modelFile ++ ".sp.1"
        clsrFile1 = modelFile ++ ".clsr.1"


learn :: V2 -> ByteString -> Text -> IO ()
learn V2 {..} str label = do
  Encoder.encode str modelEncSdr modelEncoder
  SP.compute modelEncSdr True modelSpSdr modelSp
  idx <- getLabelIdx labelHandle label
  Clsr.learn modelSpSdr idx modelClsr

infer_ :: V2 -> ByteString -> Int -> IO [Double]
infer_ V2 {..} str size = do
  Encoder.encode str modelEncSdr modelEncoder
  SP.compute modelEncSdr False modelSpSdr modelSp
  Clsr.infer modelSpSdr size modelClsr


getV2Opts :: FilePath -> IO V2Opts
getV2Opts fn = do
  eOpts <- decodeFileEither fn
  case eOpts of
    Left _     -> pure v2Opts
    Right opts -> pure opts


loadModel :: Maybe FilePath -> FilePath -> IO Model
loadModel mBootFile modelFile = do
  opts <- getV2Opts $ bootFile ++ ".opts.yml"
  v2 <- loadV2 opts bootFile

  pure Model
    { modelSave = saveV2 v2
    , modelLearn = learn v2
    , modelInfer = infer_ v2
    , modelLabels = labelHandle v2
    }

  where bootFile = fromMaybe modelFile mBootFile
