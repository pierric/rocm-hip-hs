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
    withHipDeviceMem,
    devicePtrAsRaw,
) where

import qualified ROCm.HIP.Internal as Internal
import ROCm.HIP.Internal (HipError(..), HipArrayFormat(..), HipArrayDescriptor(..))
import ROCm.HIP.TH
import Control.Exception (bracket)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CSize)

$(checked 'Internal.hipGetDeviceCount)
$(checked 'Internal.hipGetDevice)
$(checked 'Internal.hipSetDevice)
$(checked 'Internal.hipDeviceSynchronize)

$(checked 'Internal.hipMallocRaw)

hipMalloc :: CSize -> IO Internal.HipDeviceptr
hipMalloc size = do
  ptr <- hipMallocRaw size
  return $ Internal.HipDeviceptr $ castPtr ptr

$(checked 'Internal.hipFreeRaw)

hipFree :: Internal.HipDeviceptr -> IO ()
hipFree = hipFreeRaw . devicePtrAsRaw

withHipDeviceMem :: CSize -> (Internal.HipDeviceptr -> IO a) -> IO a
withHipDeviceMem size = bracket (hipMalloc size) (hipFree)

devicePtrAsRaw :: Internal.HipDeviceptr -> Ptr ()
devicePtrAsRaw (Internal.HipDeviceptr ptr) = castPtr ptr

$(checked 'Internal.hipMemcpy)
$(checked 'Internal.hipMemcpyWithStream)
$(checked 'Internal.hipMemcpyHtoD)
$(checked 'Internal.hipMemcpyDtoH)
$(checked 'Internal.hipMemcpyDtoD)
$(checked 'Internal.hipArrayCreate)

$(checked 'Internal.hipStreamCreate)
$(checked 'Internal.hipStreamSynchronize)

$(checked 'Internal.hipModuleLoad)
$(checked 'Internal.hipModuleGetFunction)
$(checked 'Internal.hipModuleLaunchKernel)

$(checked 'Internal.hipLaunchKernel)
