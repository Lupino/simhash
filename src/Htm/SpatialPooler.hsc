{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Htm.SpatialPooler
  ( CSpatialPooler
  , SpatialPooler
  , SpatialPoolerParams (..)
  , params
  , new
  , initialize
  , withSpatialPooler
  , compute
  ) where


import           Control.Exception     (mask_)
import           Foreign.C.Types
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Marshal.Utils (fromBool)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Htm.Sdr               (CSdr, Sdr, withSdr)

#include "sdr.h"

data CSpatialPooler

foreign import ccall "new_spatialPooler" c_new :: IO (Ptr CSpatialPooler)
foreign import ccall "&delete_spatialPooler" c_delete :: FunPtr (Ptr CSpatialPooler -> IO ())
foreign import ccall "spatialPooler_compute" c_compute
  :: Ptr CSdr -> CBool -> Ptr CSdr -> Ptr CSpatialPooler -> IO ()
foreign import ccall "spatialPooler_initialize" c_initialize
  :: Ptr CInt -> CInt
  -- int * inputDimensions_ptr, int input_len,
  -> Ptr CInt -> CInt
  -- int * columnDimensions_ptr, int column_len,
  -> CInt -> CDouble
  -- int potentialRadius, float potentialPct,
  -> CBool -> CDouble
  -- bool globalInhibition, float localAreaDensity,
  -> CInt -> CInt
  -- int numActiveColumnsPerInhArea, int stimulusThreshold,
  -> CDouble -> CDouble
  -- float synPermInactiveDec, float synPermActiveInc,
  -> CDouble -> CDouble
  -- float synPermConnected, float minPctOverlapDutyCycles,
  -> CInt -> CDouble
  -- int dutyCyclePeriod, float boostStrength,
  -> CInt -> CInt -> CBool
  -- int seed, int spVerbosity, bool wrapAround,
  -> Ptr CSpatialPooler -> IO ()

data SpatialPoolerParams = SpatialPoolerParams
  { inputDimensions            :: [Int]
  --   const vector<UInt>& inputDimensions,
  , columnDimensions           :: [Int]
  --   const vector<UInt>& columnDimensions,
  , potentialRadius            :: Int
  --   UInt potentialRadius = 16u,
  , potentialPct               ::Double
  --   Real potentialPct = 0.5f,
  , globalInhibition           :: Bool
  --   bool globalInhibition = true,
  , localAreaDensity           :: Double
  --   Real localAreaDensity = 0.05f,
  , numActiveColumnsPerInhArea :: Int
  --   UInt numActiveColumnsPerInhArea = 0,
  , stimulusThreshold          ::  Int
  --   UInt stimulusThreshold = 0u,
  , synPermInactiveDec         :: Double
  --   Real synPermInactiveDec = 0.01f,
  , synPermActiveInc           :: Double
  --   Real synPermActiveInc = 0.1f,
  , synPermConnected           :: Double
  --   Real synPermConnected = 0.1f,
  , minPctOverlapDutyCycles    :: Double
  --   Real minPctOverlapDutyCycles = 0.001f,
  , dutyCyclePeriod            :: Int
  --   UInt dutyCyclePeriod = 1000u,
  , boostStrength              :: Double
  --   Real boostStrength = 0.0f,
  , seed                       :: Int
  --   Int seed = 1,
  , spVerbosity                :: Int
  --   UInt spVerbosity = 0u,
  , wrapAround                 :: Bool
  --   bool wrapAround = true
  }

params :: [Int] -> [Int] -> SpatialPoolerParams
params inputDims columnDims = SpatialPoolerParams
  { inputDimensions = inputDims
  , columnDimensions = columnDims
  ,  potentialRadius = 16
  ,  potentialPct = 0.5
  ,  globalInhibition = True
  ,  localAreaDensity = 0.05
  ,  numActiveColumnsPerInhArea = 0
  ,  stimulusThreshold = 0
  ,  synPermInactiveDec = 0.01
  ,  synPermActiveInc = 0.1
  ,  synPermConnected = 0.1
  ,  minPctOverlapDutyCycles = 0.001
  ,  dutyCyclePeriod = 1000
  ,  boostStrength = 0.0
  ,  seed = 1
  ,  spVerbosity = 0
  ,  wrapAround = True
  }

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

initialize :: SpatialPoolerParams -> SpatialPooler -> IO ()
initialize SpatialPoolerParams {..} sp =
  withArray (map fromIntegral inputDimensions) $ \inputDimensionsPtr ->
  withArray (map fromIntegral columnDimensions) $ \columnDimensionsPtr ->
  withSpatialPooler sp $ c_initialize
    inputDimensionsPtr (fromIntegral (length inputDimensions))
    columnDimensionsPtr (fromIntegral (length columnDimensions))
    (fromIntegral potentialRadius)
    (realToFrac potentialPct)
    (fromBool globalInhibition)
    (realToFrac localAreaDensity)
    (fromIntegral numActiveColumnsPerInhArea)
    (fromIntegral stimulusThreshold)
    (realToFrac synPermInactiveDec)
    (realToFrac synPermActiveInc)
    (realToFrac synPermConnected)
    (realToFrac minPctOverlapDutyCycles)
    (fromIntegral dutyCyclePeriod)
    (realToFrac boostStrength)
    (fromIntegral seed)
    (fromIntegral spVerbosity)
    (fromBool wrapAround)
