module Main where

import Foreign.C.Types
import Foreign.Ptr
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as V
import Control.Monad (when)
import System.Posix.Files (fileExist)
import ROCm.HIP

kernelFile :: String
kernelFile = "test/saxpy.hsaco"

main :: IO ()
main = do
  exist <- fileExist kernelFile
  when (not exist) $
    error $ "Kernel file '" ++ kernelFile ++ "' doesn't exist. Compile it with the command: \n" ++
      "  hipcc -O3 --genco --offload-arch=gfx1100 test/saxpy.hip -o test/saxpy.hsaco"

  let size = 1000
  let size_bytes = size * 4 :: CSize
  let block_size = 256 :: CUInt
  let grid_size = (fromIntegral size + 255) `div` block_size

  -- input 1: 1,2,3,4, ...
  -- input 2: 1,3,5,7, ...
  let hx = V.iterateN (fromIntegral size) (+1) 1 :: Vector Float
  let hy = V.iterateN (fromIntegral size) (+2) 1 :: Vector Float

  mod <- hipModuleLoad kernelFile 
  fun <- hipModuleGetFunction mod "saxpy"

  withHipDeviceMem size_bytes $ \dx ->
    withHipDeviceMem size_bytes $ \dy ->
    V.unsafeWith hx $ \px ->
    V.unsafeWith hy $ \py -> do
      hipMemcpyHtoD dx (castPtr px) size_bytes
      hipMemcpyHtoD dy (castPtr py) size_bytes
      let args = [KF 2.0, KP dx, KP dy, KI (fromIntegral size)]
      hipModuleLaunchKernel fun (Dim3 grid_size 1 1) (Dim3 block_size 1 1) 0 Nothing args
      out <- MV.new (fromIntegral size)
      MV.unsafeWith out $ \pout -> hipMemcpyDtoH (castPtr pout) dy size_bytes
      out <- V.freeze out :: IO (Vector Float)
      -- expected output: 3, 7, 11, 15, 19, ...
      print $ V.take 10 out
