module ModuleSpec where

import Control.Monad
import ROCm.HIP
import System.Posix.Files (fileExist)
import System.Process (callCommand)
import Test.Hspec

kernelFile :: String
kernelFile = "test/saxpy.hsaco"

spec :: Spec
spec = beforeAll_ ensureKernelCompiled $ do
  describe "module" $ do
    it "load" $ do
      void $ hipModuleLoad kernelFile

ensureKernelCompiled :: IO ()
ensureKernelCompiled = do
  exist <- fileExist kernelFile
  when (not exist) $ do
    callCommand "hipcc -O3 --genco --offload-arch=gfx1100 test/saxpy.hip -o test/saxpy.hsaco"
