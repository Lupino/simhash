{-# LANGUAGE OverloadedStrings #-}

module Htm.Classifier
  ( CClassifier
  , Classifier
  , new
  , withClassifier
  , learn
  , infer

  , saveToFile
  , loadFromFile
  ) where


import           Control.Exception     (mask_)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.C.Types
import           Foreign.C.String
import           Htm.Sdr               (CSdr, Sdr, withSdr)

#include "sdr.h"

data CClassifier

foreign import ccall "newCClassifier" newCClassifier :: IO (Ptr CClassifier)
foreign import ccall "&deleteCClassifier" deleteCClassifier :: FunPtr (Ptr CClassifier -> IO ())
foreign import ccall "cClassifierLearn" cClassifierLearn :: Ptr CSdr -> CInt -> Ptr CClassifier -> IO ()
foreign import ccall "cClassifierInfer" cClassifierInfer :: Ptr CSdr -> Ptr CDouble -> Ptr CClassifier -> IO ()
foreign import ccall "cClassifierSaveToFile" cClassifierSaveToFile :: CString -> CInt -> Ptr CClassifier -> IO ()
foreign import ccall "cClassifierLoadFromFile" cClassifierLoadFromFile :: CString -> CInt -> Ptr CClassifier -> IO ()

newtype Classifier = Classifier (ForeignPtr CClassifier)

new :: IO Classifier
new = mask_ $ do
  ptr <- newCClassifier
  Classifier <$> newForeignPtr deleteCClassifier ptr

withClassifier :: Classifier -> (Ptr CClassifier -> IO a) -> IO a
withClassifier (Classifier fptr) = withForeignPtr fptr

learn :: Sdr -> Int -> Classifier -> IO ()
learn sdr categoryIdx clsr =
  withSdr sdr $ \sdrPtr ->
    withClassifier clsr $
        cClassifierLearn sdrPtr (fromIntegral categoryIdx)

infer :: Sdr -> Int -> Classifier -> IO [Double]
infer sdr size clsr =
  withSdr sdr $ \sdrPtr ->
    allocaArray size $ \out -> do
      withClassifier clsr $ cClassifierInfer sdrPtr out
      map realToFrac <$> peekArray size out

saveToFile :: FilePath -> Classifier -> IO ()
saveToFile fn clsr =
  withCStringLen fn $ \(bsFn, len) ->
  withClassifier clsr $ cClassifierSaveToFile bsFn (fromIntegral len)

loadFromFile :: FilePath -> Classifier -> IO ()
loadFromFile fn clsr =
  withCStringLen fn $ \(bsFn, len) ->
  withClassifier clsr $ cClassifierLoadFromFile bsFn (fromIntegral len)
