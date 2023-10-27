{-# LANGUAGE OverloadedStrings #-}

module Htm.Classifier
  ( CClassifier
  , Classifier
  , new
  , withClassifier
  , learn
  , infer

  -- , saveToFile
  -- , loadFromFile
  ) where


import           Control.Exception     (mask_)
-- import           Data.ByteString       (ByteString)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.C.Types
import           Htm.Sdr               (CSdr, Sdr, withSdr)
-- import           Htm.Utils             (toBS)
-- import qualified Language.C.Inline.Cpp as C
#include "sdr.h"

data CClassifier
-- C.context (C.cppCtx <> C.bsCtx <> C.cppTypePairs
--   [ ("htm::Classifier", [t|CClassifier|])
--   , ("htm::SDR", [t|CSdr|])
--   ])
-- C.include "<htm/algorithms/SDRClassifier.hpp>"

foreign import ccall "newCClassifier" newCClassifier :: IO (Ptr CClassifier)
foreign import ccall "&deleteCClassifier" deleteCClassifier :: FunPtr (Ptr CClassifier -> IO ())
foreign import ccall "learn" cClassifierLearn :: Ptr CSdr -> CInt -> Ptr CClassifier -> IO ()
foreign import ccall "infer" cClassifierInfer :: Ptr CSdr -> Ptr CDouble -> Ptr CClassifier -> IO ()

-- cClassifierSaveToFile :: ByteString -> Ptr CClassifier -> IO ()
-- cClassifierSaveToFile fn ptr = do
--   [C.block| void {
--     std::string fn($bs-ptr:fn);
--     fn.resize($bs-len:fn);
--     $(htm::Classifier* ptr)->saveToFile(fn);
--   }|]
--
--
-- cClassifierLoadFromFile :: ByteString -> Ptr CClassifier -> IO ()
-- cClassifierLoadFromFile fn ptr = do
--   [C.block| void {
--     std::string fn($bs-ptr:fn);
--     fn.resize($bs-len:fn);
--     $(htm::Classifier* ptr)->loadFromFile(fn);
--   }|]

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


-- saveToFile :: FilePath -> Classifier -> IO ()
-- saveToFile fn clsr = withClassifier clsr $ cClassifierSaveToFile bsFn
--   where bsFn = toBS fn
--
--
-- loadFromFile :: FilePath -> Classifier -> IO ()
-- loadFromFile fn clsr = withClassifier clsr $ cClassifierLoadFromFile bsFn
--   where bsFn = toBS fn
