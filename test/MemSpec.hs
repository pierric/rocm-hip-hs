module MemSpec where

import Test.Hspec
import ROCm.HIP

spec :: Spec
spec = do
  describe "memory" $ do
    it "allocate/free small" $ do
      ptr <- hipMalloc 10
      hipFree ptr

    it "allocate/free large" $ do
      ptr <- hipMalloc 1000000
      hipFree ptr

  describe "array" $ do
    it "allocate array valid" $ do
      -- not documented at all, but valid num_channels are 1,2,4 only
      hipArrayCreate (HipArrayDescriptor 32 32 4 HipAdFormatFloat)
      return ()

    it "allocate array w/ bad dim" $ do
      hipArrayCreate (HipArrayDescriptor 32 32 3 HipAdFormatFloat) `shouldThrow` anyErrorCall

