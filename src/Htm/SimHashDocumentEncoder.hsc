{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.SimHashDocumentEncoder
  ( CSimHashDocumentEncoder
  , SimHashDocumentEncoder
  , SimHashDocumentEncoderOpts (..)
  , new
  , initialize
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
foreign import ccall "new_simHashDocumentEncoder" c_new :: IO (Ptr CSimHashDocumentEncoder)
foreign import ccall "&delete_simHashDocumentEncoder" c_delete :: FunPtr (Ptr CSimHashDocumentEncoder -> IO ())
foreign import ccall "simHashDocumentEncoder_initialize" c_initialize :: CInt -> CDouble -> CBool -> (Ptr CSimHashDocumentEncoder) -> IO ()
foreign import ccall "simHashDocumentEncoder_encode" c_encode
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

new :: IO SimHashDocumentEncoder
new = mask_ $ do
  ptr <- c_new
  SimHashDocumentEncoder <$> newForeignPtr c_delete ptr

withSimHashDocumentEncoder :: SimHashDocumentEncoder -> (Ptr CSimHashDocumentEncoder -> IO a) -> IO a
withSimHashDocumentEncoder (SimHashDocumentEncoder fptr) = withForeignPtr fptr

encode :: ByteString -> Sdr -> SimHashDocumentEncoder -> IO ()
encode str sdr encoder =
  B.useAsCStringLen str $ \(cstr, len) ->
  withSdr sdr $ \sdrPtr ->
    withSimHashDocumentEncoder encoder $
      c_encode cstr (fromIntegral len) sdrPtr


initialize :: SimHashDocumentEncoderOpts -> SimHashDocumentEncoder -> IO ()
initialize SimHashDocumentEncoderOpts {..} encoder =
  withSimHashDocumentEncoder encoder $
    c_initialize (fromIntegral optSize) (realToFrac optSparsity) (fromBool optTokenSimilarity)
