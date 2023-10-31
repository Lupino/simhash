{-# LANGUAGE OverloadedStrings #-}

module Htm.SpatialPooler
  ( CSpatialPooler
  , SpatialPooler
  , new
  , initialize
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

foreign import ccall "new_spatialPooler" c_new :: IO (Ptr CSpatialPooler)
foreign import ccall "&delete_spatialPooler" c_delete :: FunPtr (Ptr CSpatialPooler -> IO ())
foreign import ccall "spatialPooler_compute" c_compute
  :: Ptr CSdr -> CBool -> Ptr CSdr -> Ptr CSpatialPooler -> IO ()
foreign import ccall "spatialPooler_initialize" c_initialize :: CInt -> CInt -> Ptr CSpatialPooler -> IO ()

newtype SpatialPooler = SpatialPooler (ForeignPtr CSpatialPooler)

new :: IO SpatialPooler
new = mask_ $ do
  ptr <- c_new
  SpatialPooler <$> newForeignPtr c_delete ptr

withSpatialPooler :: SpatialPooler -> (Ptr CSpatialPooler -> IO a) -> IO a
withSpatialPooler (SpatialPooler fptr) = withForeignPtr fptr

compute :: Sdr -> Bool -> Sdr -> SpatialPooler -> IO ()
compute input learn active sp =
  withSdr input $ \inputPtr ->
    withSdr active $ \activePtr ->
      withSpatialPooler sp $ c_compute inputPtr (fromBool learn) activePtr

initialize :: Int -> Int -> SpatialPooler -> IO ()
initialize inputDim outputDim sp =
  withSpatialPooler sp $ c_initialize (fromIntegral inputDim) (fromIntegral outputDim)
