module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector.HIP.Mutable as VHM
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import ROCm.HIP
import ROCm.HIP.Function

kernelFile :: String
kernelFile = "test/saxpy.hsaco"

saxpy :: String -> String -> Dim3 -> Dim3 -> Float -> HipDeviceptr -> HipDeviceptr -> Int -> Hip IO ()
saxpy kernel name grid block a v1 v2 sz = do
  let args = [KF a, KP v1, KP v2, KI (fromIntegral sz)]
  fun <- loadFunction kernel name
  liftIO $ hipModuleLaunchKernel fun grid block 0 Nothing args

main :: IO ()
main = runHip $ do
  let size = 1000
  let block_size = 256 :: CUInt
  let grid_size = (fromIntegral size + 255) `div` block_size

  -- input 1: 1,2,3,4, ...
  -- input 2: 1,3,5,7, ...
  hx <- liftIO $ VHM.generate size ((+ 1) . fromIntegral)
  hy <- liftIO $ VHM.generate size ((+ 1) . (* 2) . fromIntegral)

  VHM.unsafeWith (hx :: VHM.IOVector Float) $ \dx ->
    VHM.unsafeWith (hy :: VHM.IOVector Float) $ \dy -> do
      let args = [KF 2.0, KP dx, KP dy, KI (fromIntegral size)]
      saxpy kernelFile "saxpy" (Dim3 grid_size 1 1) (Dim3 block_size 1 1) 2.0 dx dy size

  liftIO $ do
    out <- V.freeze =<< VHM.copyToHost hy
    -- expected output: 3, 7, 11, 15, 19, ...
    print $ V.take 10 out
