{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Htm.SpatialPooler
  ( CSpatialPooler
  , SpatialPooler
  , new
  , withSpatialPooler
  , compute

  , saveToFile
  , loadFromFile
  ) where


import           Control.Exception     (mask_)
import           Data.ByteString       (ByteString)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Utils (fromBool)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Htm.Sdr               (CSdr, Sdr, withSdr)
import           Htm.Utils             (toBS)
import qualified Language.C.Inline.Cpp as C

data CSpatialPooler
C.context (C.cppCtx <> C.bsCtx <> C.cppTypePairs
  [ ("htm::SpatialPooler", [t|CSpatialPooler|])
  , ("htm::SDR", [t|CSdr|])
  ])
C.include "<htm/algorithms/SpatialPooler.hpp>"

newCSpatialPooler :: C.CInt -> C.CInt -> IO (Ptr CSpatialPooler)
newCSpatialPooler inputDim columnDim =
  [C.block| htm::SpatialPooler* {
    return new htm::SpatialPooler({(htm::UInt)$(int inputDim)}, {(htm::UInt)$(int columnDim)});
  }|]

deleteCSpatialPooler :: FunPtr (Ptr CSpatialPooler -> IO ())
deleteCSpatialPooler =
  [C.funPtr|void deleteSpatialPooler(htm::SpatialPooler* sdr){delete sdr;}|]

cSpatialPoolerCompute :: Ptr CSdr -> C.CBool -> Ptr CSdr -> Ptr CSpatialPooler -> IO ()
cSpatialPoolerCompute inputPtr learn activePtr ptr =
  [C.block| void {
    $(htm::SpatialPooler* ptr)->compute(*$(htm::SDR* inputPtr), $(bool learn), *$(htm::SDR* activePtr));
  }|]


cSpatialPoolerSaveToFile :: ByteString -> Ptr CSpatialPooler -> IO ()
cSpatialPoolerSaveToFile fn ptr = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(htm::SpatialPooler* ptr)->saveToFile(fn);
  }|]


cSpatialPoolerLoadFromFile :: ByteString -> Ptr CSpatialPooler -> IO ()
cSpatialPoolerLoadFromFile fn ptr = do
  [C.block| void {
    std::string fn($bs-ptr:fn);
    fn.resize($bs-len:fn);
    $(htm::SpatialPooler* ptr)->loadFromFile(fn);
  }|]

newtype SpatialPooler = SpatialPooler (ForeignPtr CSpatialPooler)

new :: Int -> Int -> IO SpatialPooler
new inputDim outputDim = mask_ $ do
  ptr <- newCSpatialPooler (fromIntegral inputDim) (fromIntegral outputDim)
  SpatialPooler <$> newForeignPtr deleteCSpatialPooler ptr

withSpatialPooler :: SpatialPooler -> (Ptr CSpatialPooler -> IO a) -> IO a
withSpatialPooler (SpatialPooler fptr) = withForeignPtr fptr

compute :: Sdr -> Bool -> Sdr -> SpatialPooler -> IO ()
compute input learn active sp =
  withSdr input $ \inputPtr ->
    withSdr active $ \activePtr ->
      withSpatialPooler sp $
        cSpatialPoolerCompute inputPtr (fromBool learn) activePtr


saveToFile :: FilePath -> SpatialPooler -> IO ()
saveToFile fn sp = withSpatialPooler sp $ cSpatialPoolerSaveToFile bsFn
  where bsFn = toBS fn


loadFromFile :: FilePath -> SpatialPooler -> IO ()
loadFromFile fn sp = withSpatialPooler sp $ cSpatialPoolerLoadFromFile bsFn
  where bsFn = toBS fn
