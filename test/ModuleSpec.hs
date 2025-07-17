module ModuleSpec where

import Control.Monad
import Data.Int
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Text.Printf
import Test.Hspec
import ROCm.HIP

spec :: Spec
spec = do
  describe "module" $ do
    it "load" $ do
      void $ hipModuleLoad "/home2/jiasen/workspace/hip-hs/rocm-hip/test/saxpy.hsaco"
