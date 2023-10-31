{-# LANGUAGE CPP #-}

module Htm.Sdr
  ( CSdr
  , Sdr
  , new
  , initialize
  , withSdr
  ) where


import           Control.Exception     (mask_)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)

import           Foreign.C.Types
import           Foreign.Marshal.Array (withArray)
import           Foreign.Ptr           (FunPtr, Ptr)


#include "sdr.h"

data CSdr

foreign import ccall "new_sdr" c_new_sdr :: IO (Ptr CSdr)
foreign import ccall "&delete_sdr" c_delete_sdr :: FunPtr (Ptr CSdr -> IO ())
foreign import ccall "sdr_initialize" c_sdr_initialize :: Ptr CInt -> CInt -> Ptr CSdr -> IO ()

newtype Sdr = Sdr (ForeignPtr CSdr)

new :: IO Sdr
new = mask_ $ do
  ptr <- c_new_sdr
  Sdr <$> newForeignPtr c_delete_sdr ptr

withSdr :: Sdr -> (Ptr CSdr -> IO a) -> IO a
withSdr (Sdr fptr) = withForeignPtr fptr

initialize :: [Int] -> Sdr -> IO ()
initialize dims sdr =
  withArray (map fromIntegral dims) $ \dimsPtr ->
  withSdr sdr $ c_sdr_initialize dimsPtr len

  where len = fromIntegral $ length dims
