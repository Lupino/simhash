{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Htm.SimHashDocumentEncoder
  ( CSimHashDocumentEncoder
  , SimHashDocumentEncoder
  , SimHashDocumentEncoderOpts (..)
  , new
  , withSimHashDocumentEncoder
  , encode
  ) where


import           Control.Exception     (mask_)
import           Data.Aeson            (FromJSON, parseJSON, withObject, (.!=),
                                        (.:?))
import           Data.ByteString       (ByteString)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Utils (fromBool)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Htm.Sdr               (CSdr, Sdr, withSdr)
import qualified Language.C.Inline.Cpp as C


data CSimHashDocumentEncoder
C.context (C.cppCtx <> C.bsCtx <> C.cppTypePairs
  [ ("htm::SimHashDocumentEncoder", [t|CSimHashDocumentEncoder|])
  , ("htm::SDR", [t|CSdr|])
  ])
C.include "<htm/encoders/SimHashDocumentEncoder.hpp>"

newCSimHashDocumentEncoder :: C.CInt -> C.CDouble -> C.CBool -> IO (Ptr CSimHashDocumentEncoder)
newCSimHashDocumentEncoder size sparsity tokenSimilarity =
  [C.block| htm::SimHashDocumentEncoder* {
    htm::SimHashDocumentEncoderParameters params;
    params.size = $(int size);
    params.sparsity = $(double sparsity);
    params.tokenSimilarity = $(bool tokenSimilarity);
    return new htm::SimHashDocumentEncoder(params);
  }|]

deleteCSimHashDocumentEncoder :: FunPtr (Ptr CSimHashDocumentEncoder -> IO ())
deleteCSimHashDocumentEncoder =
  [C.funPtr|void deleteSimHashDocumentEncoder(htm::SimHashDocumentEncoder* sdr){delete sdr;}|]

cSimHashDocumentEncoderEncode :: ByteString -> Ptr CSdr -> Ptr CSimHashDocumentEncoder -> IO ()
cSimHashDocumentEncoderEncode str sdrPtr ptr =
  [C.block| void {
    std::string str($bs-ptr:str);
    str.resize($bs-len:str);
    $(htm::SimHashDocumentEncoder* ptr)->encode(str, *$(htm::SDR* sdrPtr));
  }|]

newtype SimHashDocumentEncoder = SimHashDocumentEncoder (ForeignPtr CSimHashDocumentEncoder)

data SimHashDocumentEncoderOpts = SimHashDocumentEncoderOpts
  { optSize            :: Int
  , optSparsity        :: Double
  , optTokenSimilarity :: Bool
  }
  deriving Show

instance FromJSON SimHashDocumentEncoderOpts where
  parseJSON = withObject "SimHashDocumentEncoderOpts" $ \o -> do
    optSize            <- o .:? "size" .!= 600
    optSparsity        <- o .:? "sparsity" .!= 0.2
    optTokenSimilarity <- o .:? "token_similarity" .!= True
    return SimHashDocumentEncoderOpts {..}

new :: SimHashDocumentEncoderOpts -> IO SimHashDocumentEncoder
new SimHashDocumentEncoderOpts {..} = mask_ $ do
  ptr <- newCSimHashDocumentEncoder (fromIntegral optSize) (realToFrac optSparsity) (fromBool optTokenSimilarity)
  SimHashDocumentEncoder <$> newForeignPtr deleteCSimHashDocumentEncoder ptr

withSimHashDocumentEncoder :: SimHashDocumentEncoder -> (Ptr CSimHashDocumentEncoder -> IO a) -> IO a
withSimHashDocumentEncoder (SimHashDocumentEncoder fptr) = withForeignPtr fptr

encode :: ByteString -> Sdr -> SimHashDocumentEncoder -> IO ()
encode str sdr encoder =
  withSdr sdr $ \sdrPtr ->
    withSimHashDocumentEncoder encoder $
      cSimHashDocumentEncoderEncode str sdrPtr
