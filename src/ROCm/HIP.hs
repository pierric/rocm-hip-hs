{-# LANGUAGE TemplateHaskell #-}

module ROCm.HIP (
    hipGetDeviceCount,
    hipGetDevice,
    hipSetDevice,
    hipDeviceSynchronize,
    hipMalloc,
    hipFree,
    hipMemcpy,
    hipMemcpyHtoD,
    hipMemcpyDtoH,
    hipMemcpyDtoD,
    hipMemcpyWithStream,
    hipMemGetInfo,
    hipMemPtrGetInfo,
    hipArrayCreate,
    hipStreamCreate,
    hipStreamSynchronize,
    hipModuleLoad,
    hipModuleGetFunction,
    hipModuleLaunchKernel,
    hipLaunchKernel,
    HipError(..),
    HipArrayFormat(..),
    HipArrayDescriptor(..),
    HipMemcpyKind(..),
    withHipDeviceMem,
    Runtime.devicePtrAsRaw,
    HipDeviceptr,
    Dim3(..),
    KernelArg(..),
) where

import qualified ROCm.HIP.Runtime as Runtime
import ROCm.HIP.Runtime (HipError(..), HipArrayFormat(..), HipArrayDescriptor(..), HipMemcpyKind(..), HipDeviceptr, Dim3(..))
import ROCm.HIP.TH
import Control.Exception (bracket)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.C.Types (CSize)
import Foreign.Storable
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Blaze.ByteString.Builder (toByteString, fromStorable, fromWord8)
import Data.Int

$(checked 'Runtime.hipGetDeviceCount)
$(checked 'Runtime.hipGetDevice)
$(checked 'Runtime.hipSetDevice)
$(checked 'Runtime.hipDeviceSynchronize)

$(checked 'Runtime.hipMallocRaw)

hipMalloc :: CSize -> IO Runtime.HipDeviceptr
hipMalloc size = do
  ptr <- hipMallocRaw size
  return $ Runtime.HipDeviceptr $ castPtr ptr

$(checked 'Runtime.hipFree)

withHipDeviceMem :: CSize -> (Runtime.HipDeviceptr -> IO a) -> IO a
withHipDeviceMem size = bracket (hipMalloc size) (hipFree)

$(checked 'Runtime.hipMemcpy)
$(checked 'Runtime.hipMemcpyWithStream)
$(checked 'Runtime.hipMemcpyHtoD)
$(checked 'Runtime.hipMemcpyDtoH)
$(checked 'Runtime.hipMemcpyDtoD)
$(checked 'Runtime.hipMemGetInfo)
$(checked 'Runtime.hipMemPtrGetInfo)

$(checked 'Runtime.hipArrayCreate)

$(checked 'Runtime.hipStreamCreate)
$(checked 'Runtime.hipStreamSynchronize)

$(checked 'Runtime.hipModuleLoad)
$(checked 'Runtime.hipModuleGetFunction)
$(checked 'Runtime.hipModuleLaunchKernelRaw)

$(checked 'Runtime.hipLaunchKernel)

data KernelArg = KI Int32 | KF Float | KP HipDeviceptr

pack :: [KernelArg] -> ByteString
pack args = do
  let alignmentFinal = foldr lcm 1 (map alignmentK args)
  toByteString $ mconcat $ concatMap (build alignmentFinal) args

  where
  alignmentK (KI v) = alignment v
  alignmentK (KF v) = alignment v
  alignmentK (KP v) = alignment v

  build a (KI v) = [fromStorable v] ++ padding (a - sizeOf v)
  build a (KF v) = [fromStorable v] ++ padding (a - sizeOf v)
  build a (KP v) = [fromStorable v] ++ padding (a - sizeOf v)

  padding n = replicate n (fromWord8 0)

hipModuleLaunchKernel :: Runtime.HipFunction
                      -> Dim3
                      -> Dim3
                      -> Int
                      -> Maybe Runtime.HipStream
                      -> [KernelArg]
                      -> IO ()
hipModuleLaunchKernel func grid block shm stream args = do
  let Dim3 gridX gridY gridZ = grid
      Dim3 blockX blockY blockZ = block
  unsafeUseAsCStringLen (pack args) $ \(ptr, len) -> do
    Runtime.withParamConfig (castPtr ptr) len $ \param -> do
      hipModuleLaunchKernelRaw func gridX gridY gridZ blockX blockY blockZ (fromIntegral shm) stream nullPtr param
