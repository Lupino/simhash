{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Data.ByteString       (ByteString)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Htm.Sdr               (CSdr, Sdr, withSdr)
import qualified Language.C.Inline.Cpp as C

data CClassifier
C.context (C.cppCtx <> C.bsCtx <> C.cppTypePairs
  [ ("htm::Classifier", [t|CClassifier|])
  , ("htm::SDR", [t|CSdr|])
  ])
C.include "<htm/algorithms/SDRClassifier.hpp>"

newCClassifier :: IO (Ptr CClassifier)
newCClassifier =
  [C.block| htm::Classifier* {
    return new htm::Classifier();
  }|]

deleteCClassifier :: FunPtr (Ptr CClassifier -> IO ())
deleteCClassifier =
  [C.funPtr|void deleteClassifier(htm::Classifier* sdr){delete sdr;}|]

cClassifierLearn :: Ptr CSdr -> C.CInt -> Ptr CClassifier -> IO ()
cClassifierLearn patternPtr categoryIdx ptr =
  [C.block| void {
    htm::SDR pattern($(htm::SDR* patternPtr)->dimensions);
    pattern.setDense($(htm::SDR* patternPtr)->getDense());
    $(htm::Classifier* ptr)->learn(pattern, $(int categoryIdx));
  }|]


cClassifierInfer :: Ptr CSdr -> Ptr C.CDouble -> Ptr CClassifier -> IO ()
cClassifierInfer patternPtr out ptr =
  [C.block| void {
    htm::SDR pattern($(htm::SDR* patternPtr)->dimensions);
    pattern.setDense($(htm::SDR* patternPtr)->getDense());
    htm::PDF ret = $(htm::Classifier* ptr)->infer(pattern);

    for (int i=0;i<ret.size();i++) {
        $(double *out)[i] = ret[i];
    }
  }|]


cClassifierSaveToFile :: ByteString -> Ptr CClassifier -> IO ()
cClassifierSaveToFile fn ptr = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(htm::Classifier* ptr)->saveToFile(fn);
  }|]


cClassifierLoadFromFile :: ByteString -> Ptr CClassifier -> IO ()
cClassifierLoadFromFile fn ptr = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(htm::Classifier* ptr)->loadFromFile(fn);
  }|]

newtype Classifier = Classifier (ForeignPtr CClassifier)

new :: IO Classifier
new = mask_ $ do
  ptr <- newCClassifier
  Classifier <$> newForeignPtr deleteCClassifier ptr

withClassifier :: Classifier -> (Ptr CClassifier -> IO a) -> IO a
withClassifier (Classifier fptr) = withForeignPtr fptr

learn :: Sdr -> Int -> Classifier -> IO ()
learn pattern categoryIdx clsr =
  withClassifier clsr $ \ptr ->
    withSdr pattern $ \patternPtr ->
      cClassifierLearn patternPtr (fromIntegral categoryIdx) ptr

infer :: Sdr -> Int -> Classifier -> IO [Double]
infer pattern size clsr =
  withClassifier clsr $ \ptr ->
    withSdr pattern $ \patternPtr ->
      allocaArray size $ \out -> do
        cClassifierInfer patternPtr out ptr
        map realToFrac <$> peekArray size out


saveToFile :: Classifier -> ByteString -> IO ()
saveToFile sh fn = withClassifier sh $ cClassifierSaveToFile fn


loadFromFile :: Classifier -> ByteString -> IO ()
loadFromFile sh fn = withClassifier sh $ cClassifierLoadFromFile fn
