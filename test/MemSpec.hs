module MemSpec where

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
  describe "memory alloc/free" $ do
    it "allocate/free small" $ do
      ptr <- hipMalloc 10
      hipFree ptr

    it "allocate/free large" $ do
      ptr <- hipMalloc 1000000
      hipFree ptr

  describe "memory copy host <> device" $ do
    it "host >> device >> host" $ do
      withHipDeviceMem 4 $ \dptr -> do
        with (99::Int32) $ \hptr -> do
          hipMemcpyHtoD dptr (castPtr hptr) 4
          allocaBytes 4 $ \hptr2 -> do
            hipMemcpyDtoH (castPtr hptr2) dptr 4 
            val <- peek hptr2
            (val :: Int32) `shouldBe` 99

    it "host >> device >> device >> host" $ do
      withHipDeviceMem 4 $ \dptr -> do
        withHipDeviceMem 4 $ \dptr2 -> do
          with (999::Int32) $ \hptr -> do
            hipMemcpyHtoD dptr (castPtr hptr) 4
            hipMemcpyDtoD dptr2 dptr 4
            allocaBytes 4 $ \hptr2 -> do
              hipMemcpyDtoH (castPtr hptr2) dptr2 4 
              val <- peek hptr2
              (val :: Int32) `shouldBe` 999

  describe "memory copy async" $ do
    it "host >> device >> device >> host" $ do
      stream <- hipStreamCreate
      withHipDeviceMem 4 $ \dptr -> do
        withHipDeviceMem 4 $ \dptr2 -> do
          with (999::Int32) $ \hptr -> do
            hipMemcpyWithStream (devicePtrAsRaw dptr) (castPtr hptr) 4 HipMemcpyHostToDevice stream
            hipMemcpyWithStream (devicePtrAsRaw dptr2) (devicePtrAsRaw dptr) 4 HipMemcpyDeviceToDevice stream
            allocaBytes 4 $ \hptr2 -> do
              hipMemcpyWithStream (castPtr hptr2) (devicePtrAsRaw dptr2) 4 HipMemcpyDeviceToDevice stream
              hipStreamSynchronize stream
              val <- peek hptr2
              (val :: Int32) `shouldBe` 999

  describe "memory info" $ do
    it "get info and allocate" $ do
      (free, total) <- hipMemGetInfo
      total `shouldSatisfy` (> 0)
      free `shouldSatisfy` (\v -> v < total && v > 0)

      withHipDeviceMem 4 $ \_ -> do
        (free1, total1) <- hipMemGetInfo
        total1 `shouldBe` total
        free1 `shouldSatisfy` (\v -> v < free && v > 0)

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

