{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Htm.Saver
  ( saveToFile
  , loadFromFile
  ) where


import qualified Data.Text        as T (intercalate, split, strip)
import           Data.Text         (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8  as B (useAsCStringLen, packCStringLen)
import           Foreign.Ptr       (Ptr)
import           Foreign.Marshal
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Storable
import           Htm.SpatialPooler (CSpatialPooler, withSpatialPooler, SpatialPooler)
import           Htm.Classifier    (CClassifier, withClassifier, Classifier)
import           Htm.Sdr           (CSdr, withSdr, Sdr)
import           Htm.SimHashDocumentEncoder (CSimHashDocumentEncoder, withSimHashDocumentEncoder, SimHashDocumentEncoder)
import           UnliftIO         (TVar, atomically, readTVarIO, writeTVar)

#include "sdr.h"

foreign import ccall
  "saveToFile" c_saveToFile
    :: CString -> CInt -> Ptr CSpatialPooler -> Ptr CClassifier -> Ptr CSimHashDocumentEncoder -> Ptr CSdr -> Ptr CSdr -> CString -> CInt -> IO ()
foreign import ccall
  "loadFromFile" c_loadFromFile
    :: CString -> CInt -> Ptr CSpatialPooler -> Ptr CClassifier -> Ptr CSimHashDocumentEncoder -> Ptr CSdr -> Ptr CSdr -> CString -> Ptr CInt -> IO ()

saveToFile :: FilePath -> SpatialPooler -> Classifier -> SimHashDocumentEncoder -> Sdr -> Sdr -> TVar [Text] -> IO ()
saveToFile fn pooler clsr encoder sdr0 sdr1 labelHandle = do
  labels <- encodeUtf8 . T.intercalate "\n" <$> readTVarIO labelHandle
  B.useAsCStringLen labels $ \(bsLabels, labelLen) ->
    withCStringLen fn $ \(bsFn, len) ->
    withSpatialPooler pooler $ \poolerPtr ->
    withClassifier clsr $ \clsrPtr ->
    withSimHashDocumentEncoder encoder $ \encoderPtr ->
    withSdr sdr0 $ \sdr0Ptr ->
    withSdr sdr1 $ \sdr1Ptr ->
      c_saveToFile bsFn (fromIntegral len) poolerPtr clsrPtr encoderPtr sdr0Ptr sdr1Ptr bsLabels (fromIntegral labelLen)

loadFromFile :: FilePath -> SpatialPooler -> Classifier -> SimHashDocumentEncoder -> Sdr -> Sdr -> TVar [Text] -> IO ()
loadFromFile fn pooler clsr encoder sdr0 sdr1 labelHandle =
  allocaBytes 4194304 $ \labelPtr ->
    withCStringLen fn $ \(bsFn, len) ->
    withSpatialPooler pooler $ \poolerPtr ->
    withClassifier clsr $ \clsrPtr ->
    withSimHashDocumentEncoder encoder $ \encoderPtr ->
    withSdr sdr0 $ \sdr0Ptr ->
    withSdr sdr1 $ \sdr1Ptr ->
    alloca $ \labelLenPtr -> do
      c_loadFromFile bsFn (fromIntegral len) poolerPtr clsrPtr encoderPtr sdr0Ptr sdr1Ptr labelPtr labelLenPtr
      (labelLen :: CInt) <- peek labelLenPtr
      bs <- B.packCStringLen (labelPtr, fromIntegral labelLen)
      atomically $ writeTVar labelHandle $ T.split (=='\n') $ T.strip $ decodeUtf8 bs
