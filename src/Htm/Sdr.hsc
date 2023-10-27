{-# LANGUAGE OverloadedStrings #-}

module Htm.Sdr
  ( CSdr
  , Sdr
  , new
  , withSdr
  ) where


import           Control.Exception     (mask_)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)

import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.C.Types

#include "sdr.h"

data CSdr

foreign import ccall "newCSdr" newCSdr :: CInt -> IO (Ptr CSdr)

foreign import ccall "&deleteCSdr" deleteCSdr :: FunPtr (Ptr CSdr -> IO ())

newtype Sdr = Sdr (ForeignPtr CSdr)

new :: Int -> IO Sdr
new dim = mask_ $ do
  ptr <- newCSdr (fromIntegral dim)
  Sdr <$> newForeignPtr deleteCSdr ptr

withSdr :: Sdr -> (Ptr CSdr -> IO a) -> IO a
withSdr (Sdr fptr) = withForeignPtr fptr
