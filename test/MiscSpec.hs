module MiscSpec where

import ROCm.HIP
import Test.Hspec

spec :: Spec
spec = do
  describe "device" $ do
    it "get device count" $ do
      cnt <- hipGetDeviceCount
      cnt `shouldBe` 1

    it "get current device" $ do
      dev <- hipGetDevice
      dev `shouldBe` 0

    it "set current device" $ do
      hipSetDevice 0

    it "sync" $ do
      hipDeviceSynchronize

