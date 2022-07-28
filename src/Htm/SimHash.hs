{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Htm.SimHash
  ( CSimHash
  , SimHash
  , new
  , withSimHash
  , setup
  , learn
  , infer
  , saveToFile
  , loadFromFile
  , loadFromFileV2
  ) where


import           Control.Exception     (mask_)
import           Data.ByteString       (ByteString)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Ptr           (FunPtr, Ptr)
import qualified Language.C.Inline.Cpp as C

data CSimHash

C.context (C.cppCtx <> C.bsCtx <> C.cppTypePairs [("simhash::SimHash", [t|CSimHash|])])
C.include "<SimHash.hpp>"

newCSimHash :: IO (Ptr CSimHash)
newCSimHash =
  [C.block| simhash::SimHash* {
    return new simhash::SimHash();
  }|]


deleteCSimHash :: FunPtr (Ptr CSimHash -> IO ())
deleteCSimHash =
  [C.funPtr|void deleteSimHash(simhash::SimHash* sh){delete sh;}|]


cSimHashSetup :: Ptr CSimHash -> IO ()
cSimHashSetup ptr =
  [C.exp| void {$(simhash::SimHash* ptr)->setup()}|]


cSimHashLearn :: ByteString -> C.CInt -> Ptr CSimHash -> IO ()
cSimHashLearn str idx ptr =
  [C.block| void {
    std::string str($bs-ptr:str);
    str.resize($bs-len:str);
    $(simhash::SimHash* ptr)->learn(str, $(int idx));
  }|]


cSimHashInfer :: ByteString -> Ptr C.CDouble -> Ptr CSimHash -> IO ()
cSimHashInfer str out ptr = do
  [C.block| void {
    std::string str($bs-ptr:str);
    str.resize($bs-len:str);
    $(simhash::SimHash* ptr)->infer(str, $(double* out));
  }|]

cSimHashSaveToFile :: ByteString -> Ptr CSimHash -> IO ()
cSimHashSaveToFile fn ptr = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(simhash::SimHash* ptr)->saveToFile(fn);
  }|]


cSimHashLoadFromFile :: ByteString -> Ptr CSimHash -> IO ()
cSimHashLoadFromFile fn ptr = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(simhash::SimHash* ptr)->loadFromFile(fn);
  }|]


cSimHashLoadFromFileV2 :: ByteString -> ByteString -> Ptr CSimHash -> IO ()
cSimHashLoadFromFileV2 spFile clsrFile ptr = do
  [C.block| void {
    std::string spFile($bs-ptr:spFile);
    spFile.resize($bs-len:spFile);
    std::string clsrFile($bs-ptr:clsrFile);
    clsrFile.resize($bs-len:clsrFile);
    $(simhash::SimHash* ptr)->loadFromFileV2(spFile, clsrFile);
  }|]


newtype SimHash = SimHash (ForeignPtr CSimHash)

new :: IO SimHash
new = mask_ $ do
  ptr <- newCSimHash
  SimHash <$> newForeignPtr deleteCSimHash ptr

withSimHash :: SimHash -> (Ptr CSimHash -> IO a) -> IO a
withSimHash (SimHash fptr) = withForeignPtr fptr

setup :: SimHash -> IO ()
setup sh = withSimHash sh cSimHashSetup


learn :: ByteString -> Int -> SimHash -> IO ()
learn str idx sh =
  withSimHash sh $ cSimHashLearn str (fromIntegral idx)


infer :: ByteString -> Int -> SimHash -> IO [Double]
infer str size sh =
  allocaArray size $ \out -> do
    withSimHash sh $ cSimHashInfer str out
    map realToFrac <$> peekArray size out

saveToFile :: ByteString -> SimHash -> IO ()
saveToFile fn sh = withSimHash sh $ cSimHashSaveToFile fn


loadFromFile :: ByteString -> SimHash -> IO ()
loadFromFile fn sh = withSimHash sh $ cSimHashLoadFromFile fn


loadFromFileV2 :: ByteString -> ByteString -> SimHash -> IO ()
loadFromFileV2 spFile clsrFile sh =
  withSimHash sh $ cSimHashLoadFromFileV2 spFile clsrFile
