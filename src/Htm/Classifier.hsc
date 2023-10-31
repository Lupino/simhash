{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Htm.Classifier
  ( CClassifier
  , Classifier
  , new
  , initialize
  , withClassifier
  , learn
  , infer
  ) where


import           Control.Exception     (mask_)
import           Foreign.C.Types
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Htm.Sdr               (CSdr, Sdr, withSdr)

#include "sdr.h"

data CClassifier

foreign import ccall "new_classifier" c_new_classifier :: IO (Ptr CClassifier)
foreign import ccall "&delete_classifier" c_delete_classifier :: FunPtr (Ptr CClassifier -> IO ())
foreign import ccall "classifier_learn" c_classifier_learn :: Ptr CSdr -> CInt -> Ptr CClassifier -> IO ()
foreign import ccall "classifier_infer" c_classifier_infer :: Ptr CSdr -> Ptr CDouble -> Ptr CClassifier -> IO ()
foreign import ccall "classifier_initialize" c_classifier_initialize :: CDouble -> Ptr CClassifier -> IO ()

newtype Classifier = Classifier (ForeignPtr CClassifier)

new :: IO Classifier
new = mask_ $ do
  ptr <- c_new_classifier
  Classifier <$> newForeignPtr c_delete_classifier ptr

withClassifier :: Classifier -> (Ptr CClassifier -> IO a) -> IO a
withClassifier (Classifier fptr) = withForeignPtr fptr

learn :: Sdr -> Int -> Classifier -> IO ()
learn sdr categoryIdx clsr =
  withSdr sdr $ \sdrPtr ->
    withClassifier clsr $
        c_classifier_learn sdrPtr (fromIntegral categoryIdx)

infer :: Sdr -> Int -> Classifier -> IO [Double]
infer sdr size clsr =
  withSdr sdr $ \sdrPtr ->
    allocaArray size $ \out -> do
      withClassifier clsr $ c_classifier_infer sdrPtr out
      map realToFrac <$> peekArray size out

initialize :: Double -> Classifier -> IO ()
initialize alpha clsr =
    withClassifier clsr $ c_classifier_initialize (realToFrac alpha)
