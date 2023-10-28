{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import qualified Data.ByteString.Char8  as B (useAsCStringLen)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Utils (fromBool)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.C.Types
import           Foreign.C.String      (CString)
import           Htm.Sdr               (CSdr, Sdr, withSdr)

#include "sdr.h"

data CSimHashDocumentEncoder

foreign import ccall "newCSimHashDocumentEncoder" newCSimHashDocumentEncoder :: CInt -> CDouble -> CBool -> IO (Ptr CSimHashDocumentEncoder)
foreign import ccall "&deleteCSimHashDocumentEncoder" deleteCSimHashDocumentEncoder :: FunPtr (Ptr CSimHashDocumentEncoder -> IO ())

foreign import ccall "cSimHashDocumentEncoderEncode" cSimHashDocumentEncoderEncode
  :: CString -> CInt -> Ptr CSdr -> Ptr CSimHashDocumentEncoder -> IO ()

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
  B.useAsCStringLen str $ \(cstr, len) ->
  withSdr sdr $ \sdrPtr ->
    withSimHashDocumentEncoder encoder $
      cSimHashDocumentEncoderEncode cstr (fromIntegral len) sdrPtr
