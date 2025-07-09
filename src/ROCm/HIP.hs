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
    devicePtrAsRaw,
) where

import qualified ROCm.HIP.Runtime as Runtime
import ROCm.HIP.Runtime (HipError(..), HipArrayFormat(..), HipArrayDescriptor(..), HipMemcpyKind(..))
import ROCm.HIP.TH
import Control.Exception (bracket)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CSize)

$(checked 'Runtime.hipGetDeviceCount)
$(checked 'Runtime.hipGetDevice)
$(checked 'Runtime.hipSetDevice)
$(checked 'Runtime.hipDeviceSynchronize)

$(checked 'Runtime.hipMallocRaw)

hipMalloc :: CSize -> IO Runtime.HipDeviceptr
hipMalloc size = do
  ptr <- hipMallocRaw size
  return $ Runtime.HipDeviceptr $ castPtr ptr

$(checked 'Runtime.hipFreeRaw)

hipFree :: Runtime.HipDeviceptr -> IO ()
hipFree = hipFreeRaw . devicePtrAsRaw

withHipDeviceMem :: CSize -> (Runtime.HipDeviceptr -> IO a) -> IO a
withHipDeviceMem size = bracket (hipMalloc size) (hipFree)

devicePtrAsRaw :: Runtime.HipDeviceptr -> Ptr ()
devicePtrAsRaw (Runtime.HipDeviceptr ptr) = castPtr ptr

$(checked 'Runtime.hipMemcpy)
$(checked 'Runtime.hipMemcpyWithStream)
$(checked 'Runtime.hipMemcpyHtoD)
$(checked 'Runtime.hipMemcpyDtoH)
$(checked 'Runtime.hipMemcpyDtoD)
$(checked 'Runtime.hipArrayCreate)

$(checked 'Runtime.hipStreamCreate)
$(checked 'Runtime.hipStreamSynchronize)

$(checked 'Runtime.hipModuleLoad)
$(checked 'Runtime.hipModuleGetFunction)
$(checked 'Runtime.hipModuleLaunchKernel)

$(checked 'Runtime.hipLaunchKernel)
