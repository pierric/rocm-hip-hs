{-# LANGUAGE TemplateHaskell #-}

module ROCm.HIP (
    hipGetDeviceCount,
    hipGetDevice,
    hipSetDevice,
    hipDeviceSynchronize,
    hipMalloc,
    hipFree,
    hipMemcpy,
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
) where

import qualified ROCm.HIP.Internal as Internal
import ROCm.HIP.Internal (HipError(..), HipArrayFormat(..), HipArrayDescriptor(..))
import ROCm.HIP.TH

$(checked 'Internal.hipGetDeviceCount)
$(checked 'Internal.hipGetDevice)
$(checked 'Internal.hipSetDevice)
$(checked 'Internal.hipDeviceSynchronize)

$(checked 'Internal.hipMalloc)
$(checked 'Internal.hipFree)
$(checked 'Internal.hipMemcpy)
$(checked 'Internal.hipArrayCreate)

$(checked 'Internal.hipStreamCreate)
$(checked 'Internal.hipStreamSynchronize)

$(checked 'Internal.hipModuleLoad)
$(checked 'Internal.hipModuleGetFunction)
$(checked 'Internal.hipModuleLaunchKernel)

$(checked 'Internal.hipLaunchKernel)