{-# LANGUAGE OverloadedStrings #-}

module Htm.SpatialPooler
  ( CSpatialPooler
  , SpatialPooler
  , new
  , withSpatialPooler
  , compute
  ) where


import           Control.Exception     (mask_)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Utils (fromBool)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.C.Types
import           Htm.Sdr               (CSdr, Sdr, withSdr)

#include "sdr.h"

data CSpatialPooler

foreign import ccall "newCSpatialPooler" newCSpatialPooler :: CInt -> CInt -> IO (Ptr CSpatialPooler)
foreign import ccall "&deleteCSpatialPooler" deleteCSpatialPooler :: FunPtr (Ptr CSpatialPooler -> IO ())
foreign import ccall "cSpatialPoolerCompute" cSpatialPoolerCompute
  :: Ptr CSdr -> CBool -> Ptr CSdr -> Ptr CSpatialPooler -> IO ()


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
