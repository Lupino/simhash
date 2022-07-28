{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import qualified Language.C.Inline.Cpp as C

data CSdr
C.context (C.cppCtx <> C.bsCtx <> C.cppTypePairs [("htm::SDR", [t|CSdr|])])
C.include "<htm/types/Sdr.hpp>"

newCSdr :: C.CInt -> IO (Ptr CSdr)
newCSdr dim =
  [C.block| htm::SDR* {
    return new htm::SDR({(htm::UInt)$(int dim)});
  }|]

deleteCSdr :: FunPtr (Ptr CSdr -> IO ())
deleteCSdr =
  [C.funPtr|void deleteSdr(htm::SDR* sdr){delete sdr;}|]

newtype Sdr = Sdr (ForeignPtr CSdr)

new :: Int -> IO Sdr
new dim = mask_ $ do
  ptr <- newCSdr (fromIntegral dim)
  Sdr <$> newForeignPtr deleteCSdr ptr

withSdr :: Sdr -> (Ptr CSdr -> IO a) -> IO a
withSdr (Sdr fptr) = withForeignPtr fptr
