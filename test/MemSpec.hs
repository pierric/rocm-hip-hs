module MemSpec where

import Control.Monad
import Text.Printf
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
    let asInt a = fromIntegral a :: Int
    let size = [8,128,1024]
    -- not documented at all, but valid num_channels are 1,2,4 only
    let chnl = [1,2,4]
    let fmt  = [HipAdFormatSignedInt8, HipAdFormatUnsignedInt8, HipAdFormatHalf, HipAdFormatFloat]
    forM_ [(s, c, f) | s<-size, c<-chnl, f<-fmt] $ \(s,c,f) -> do
      let title = printf "allocate array %dx%dx%d %s"
                         (asInt s) (asInt s) (asInt c) (show f)
      it title $ do
        void $ hipArrayCreate (HipArrayDescriptor s s c f)

    let chnl_bad = [0,3,5]
    forM_ [(s, c, f) | s<-size, c<-chnl_bad, f<-fmt] $ \(s,c,f) -> do
      let title = printf "allocate array (bad dim) %dx%dx%d %s"
                         (asInt s) (asInt s) (asInt c) (show f)
      it title $ do
        hipArrayCreate (HipArrayDescriptor s s c f) `shouldThrow` anyErrorCall

