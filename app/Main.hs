module Main where

import Foreign.C.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.HIP.Mutable as VHM
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
  let block_size = 256 :: CUInt
  let grid_size = (fromIntegral size + 255) `div` block_size

  -- input 1: 1,2,3,4, ...
  -- input 2: 1,3,5,7, ...
  hx <- VHM.generate size ((+1) . fromIntegral) :: IO (VHM.IOVector Float)
  hy <- VHM.generate size ((+1) . (*2) . fromIntegral) :: IO (VHM.IOVector Float)

  mod <- hipModuleLoad kernelFile 
  fun <- hipModuleGetFunction mod "saxpy"

  VHM.unsafeWith hx $ \dx ->
    VHM.unsafeWith hy $ \dy -> do
      let args = [KF 2.0, KP dx, KP dy, KI (fromIntegral size)]
      hipModuleLaunchKernel fun (Dim3 grid_size 1 1) (Dim3 block_size 1 1) 0 Nothing args
      out <- V.freeze =<< VHM.copyToHost hy
      -- expected output: 3, 7, 11, 15, 19, ...
      print $ V.take 10 out
