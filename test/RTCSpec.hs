{-# LANGUAGE MultilineStrings #-}

module RTCSpec where

import Control.Monad
import ROCm.HIP
import Test.Hspec

code :: String
code =
  """
  extern "C"
  __global__ void vector_add(float* output, float* input1, float* input2, size_t size) {
    int i = threadIdx.x;
    if (i < size) {
      output[i] = input1[i] + input2[i];
    }
  }
  """

spec :: Spec
spec = do
  describe "rtc" $ do
    it "version" $ do
      (major, minor) <- hiprtcVersion
      major `shouldSatisfy` (> 0)
      minor `shouldSatisfy` (>= 0)

    it "create" $
      void $
        hiprtcCreateProgram "prog.cu" code []

    it "compile" $ do
      prog <- hiprtcCreateProgram "prog.cu" code []
      rc <- hiprtcCompileProgram prog []
      out <- hiprtcGetProgramLog prog
      rc `shouldBe` HIPRTC_SUCCESS
      out `shouldBe` ""
