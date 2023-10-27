{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Htm.SpatialPooler
  ( CSpatialPooler
  , SpatialPooler
  , new
  , withSpatialPooler
  , compute

  -- , saveToFile
  -- , loadFromFile
  ) where


import           Control.Exception     (mask_)
-- import           Data.ByteString       (ByteString)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Utils (fromBool)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.C.Types
import           Htm.Sdr               (CSdr, Sdr, withSdr)
-- import           Htm.Utils             (toBS)

#include "sdr.h"

data CSpatialPooler

foreign import ccall "newCSpatialPooler" newCSpatialPooler :: CInt -> CInt -> IO (Ptr CSpatialPooler)
foreign import ccall "&deleteCSpatialPooler" deleteCSpatialPooler :: FunPtr (Ptr CSpatialPooler -> IO ())
foreign import ccall "cSpatialPoolerCompute" cSpatialPoolerCompute
  :: Ptr CSdr -> CBool -> Ptr CSdr -> Ptr CSpatialPooler -> IO ()

-- cSpatialPoolerSaveToFile :: ByteString -> Ptr CSpatialPooler -> IO ()
-- cSpatialPoolerSaveToFile fn ptr = do
--   [C.block| void {
--     std::string fn($bs-ptr:fn);
--     fn.resize($bs-len:fn);
--     $(htm::SpatialPooler* ptr)->saveToFile(fn);
--   }|]
--
--
-- cSpatialPoolerLoadFromFile :: ByteString -> Ptr CSpatialPooler -> IO ()
-- cSpatialPoolerLoadFromFile fn ptr = do
--   [C.block| void {
--     std::string fn($bs-ptr:fn);
--     fn.resize($bs-len:fn);
--     $(htm::SpatialPooler* ptr)->loadFromFile(fn);
--   }|]

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


-- saveToFile :: FilePath -> SpatialPooler -> IO ()
-- saveToFile fn sp = withSpatialPooler sp $ cSpatialPoolerSaveToFile bsFn
--   where bsFn = toBS fn
--
--
-- loadFromFile :: FilePath -> SpatialPooler -> IO ()
-- loadFromFile fn sp = withSpatialPooler sp $ cSpatialPoolerLoadFromFile bsFn
--   where bsFn = toBS fn
