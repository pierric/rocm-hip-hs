{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

#include "hip/hip_runtime_api.h"
#include "hip_runtime_hs.h"

module ROCm.HIP.Runtime where

import Foreign.C.Types
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable (peek, Storable(..))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafeUseAsCString)
import Blaze.ByteString.Builder (toByteString, fromStorable)

{#typedef size_t CSize#}

{#enum hipError_t as HipError {upcaseFirstLetter} deriving (Eq, Show)#}

data Dim3 = Dim3 {
  dim3X :: CUInt,
  dim3Y :: CUInt,
  dim3Z :: CUInt
} deriving (Show, Eq)

instance Storable Dim3 where
  sizeOf _ = {#sizeof dim3#}
  alignment _ = {#alignof dim3#}

  peek ptr = do
    x <- {#get dim3->x#} ptr
    y <- {#get dim3->y#} ptr
    z <- {#get dim3->z#} ptr
    return (Dim3 x y z)

  poke ptr (Dim3 x y z) = do
    {#set dim3->x#} ptr x
    {#set dim3->y#} ptr y
    {#set dim3->z#} ptr z

{#pointer *dim3 as Dim3Ptr -> Dim3#}

{#enum hipArray_Format as HipArrayFormat {underscoreToCase} deriving (Eq, Show)#}

data HipArrayDescriptor = HipArrayDescriptor {
  hip_ad_width        :: CULong,
  hip_ad_height       :: CULong,
  hip_ad_num_channels :: CUInt,
  hip_ad_format       :: HipArrayFormat
} deriving (Show, Eq)

instance Storable HipArrayDescriptor where
  sizeOf _ = {#sizeof HIP_ARRAY_DESCRIPTOR#}
  alignment _ = {#alignof HIP_ARRAY_DESCRIPTOR#}

  peek ptr = do
    w <- {#get HIP_ARRAY_DESCRIPTOR->Width#} ptr
    h <- {#get HIP_ARRAY_DESCRIPTOR->Height#} ptr
    f <- {#get HIP_ARRAY_DESCRIPTOR->Format#} ptr
    c <- {#get HIP_ARRAY_DESCRIPTOR->NumChannels#} ptr
    return (HipArrayDescriptor w h c (toEnum $ fromIntegral f))

  poke ptr (HipArrayDescriptor w h c f) = do
    {#set HIP_ARRAY_DESCRIPTOR->Width#} ptr w
    {#set HIP_ARRAY_DESCRIPTOR->Height#} ptr h
    {#set HIP_ARRAY_DESCRIPTOR->Format#} ptr (fromIntegral $ fromEnum f)
    {#set HIP_ARRAY_DESCRIPTOR->NumChannels#} ptr c

{#pointer *HIP_ARRAY_DESCRIPTOR as HipArrayDescriptorPtr -> HipArrayDescriptor#}

{#enum hipMemcpyKind as HipMemcpyKind {upcaseFirstLetter} deriving (Eq)#}

{#pointer hipDeviceptr_t as HipDeviceptr newtype#}

deriving instance Storable HipDeviceptr

{#pointer hipStream_t as HipStream foreign newtype#}

{#pointer hipArray_t as HipArray foreign newtype#}

{#fun hipGetDeviceCount as ^
  {alloca- `CInt' peek*} -> `HipError'#} 

{#fun hipGetDevice as ^
  {alloca- `CInt' peek*} -> `HipError'#} 

{#fun hipSetDevice as ^
  {`CInt'} -> `HipError'#} 

{#fun hipDeviceSynchronize as ^
  {} -> `HipError'#} 

{#fun hipMalloc as hipMalloc
  {alloca- `HipDeviceptr' peekDeviceptr*, `CSize'} -> `HipError'#} 

{#fun hipMallocAsync as hipMallocAsync
  {alloca- `HipDeviceptr' peekDeviceptr*, `CSize', `HipStream'} -> `HipError'#} 

{#fun hipFree as hipFree
  {devicePtrAsRaw `HipDeviceptr'} -> `HipError'#} 

{#fun hipMemcpy as ^
  {`Ptr ()', `Ptr ()', `CSize', `HipMemcpyKind'} -> `HipError'#} 

{#fun hipMemcpyWithStream as ^
  {`Ptr ()', `Ptr ()', `CSize', `HipMemcpyKind', `HipStream'} -> `HipError'#} 

{#fun hipMemcpyHtoD as ^
  {`HipDeviceptr', `Ptr ()', `CSize'} -> `HipError'#}

{#fun hipMemcpyDtoH as ^
  {`Ptr ()', `HipDeviceptr', `CSize'} -> `HipError'#} 

{#fun hipMemcpyDtoD as ^
  {`HipDeviceptr', `HipDeviceptr', `CSize'} -> `HipError'#} 

{#fun hipMemsetD8 as ^
  {`HipDeviceptr', id `CUChar', `CSize'} -> `HipError'#} 

{#fun hipMemsetD8Async as ^
  {`HipDeviceptr', id `CUChar', `CSize', withMaybeHipStream* `Maybe HipStream'} -> `HipError'#} 

{#fun hipMemsetD16 as ^
  {`HipDeviceptr', id `CUShort', `CSize'} -> `HipError'#} 

{#fun hipMemsetD16Async as ^
  {`HipDeviceptr', id `CUShort', `CSize', withMaybeHipStream* `Maybe HipStream'} -> `HipError'#}

{#fun hipMemsetD32 as ^
  {`HipDeviceptr', id `CInt', `CSize'} -> `HipError'#} 

{#fun hipMemsetD32Async as ^
  {`HipDeviceptr', id `CInt', `CSize', withMaybeHipStream* `Maybe HipStream'} -> `HipError'#}

{#fun hipMemPtrGetInfo as ^
  {devicePtrAsRaw `HipDeviceptr', alloca- `CSize' peek*} -> `HipError'#} 

{#fun hipMemGetInfo as ^
  {alloca- `CSize' peek*, alloca- `CSize' peek*} -> `HipError'#} 

{#fun hipArrayCreate as ^
  {alloca- `HipArray' peekHipArray*, with* `HipArrayDescriptor'} -> `HipError'#}

{#fun hipStreamCreate as ^
  {alloca- `HipStream' peekHipStream*} -> `CInt'#} 

{#fun hipStreamSynchronize as ^
  {`HipStream'} -> `HipError'#} 

{#pointer hipModule_t as HipModule foreign newtype#}

{#pointer *ihipModuleSymbol_t as HipModuleSymbol newtype#}
{#pointer hipFunction_t as HipFunction -> HipModuleSymbol #}

{#fun hipModuleLoad as hipModuleLoad
  {alloca- `HipModule' peekHipModule*, `String'} -> `HipError'#} 

{#fun hipModuleGetFunction as hipModuleGetFunction
  {alloca- `HipFunction' peek*, `HipModule', `String'} -> `HipError'#} 

{#fun hipModuleLaunchKernel as hipModuleLaunchKernelRaw
  {`HipFunction',
  `CUInt', `CUInt', `CUInt',
  `CUInt', `CUInt', `CUInt',
  `CUInt', withMaybeHipStream* `Maybe HipStream',
  id `Ptr (Ptr ())', id `Ptr (Ptr ())'} -> `HipError'#}

{#fun hipLaunchKernel_wrapped as hipLaunchKernel
  {`Ptr ()', with* `Dim3', with* `Dim3', withArray* `[Ptr ()]', `CSize', `HipStream'} -> `HipError'#}

foreign import ccall safe "Internal.chs.h &hipModuleUnload"
  hipModuleUnload :: FunPtr (Ptr HipModule -> IO ())

foreign import ccall safe "Internal.chs.h &hipStreamDestroy"
  hipStreamDestroy :: FunPtr (Ptr HipStream -> IO ())

foreign import ccall safe "Internal.chs.h &hipArrayDestroy"
  hipArrayDestroy :: FunPtr (Ptr HipArray -> IO ())

foreign import ccall safe "Internal.chs.h &hipFree"
  hipFreeAsFunPtr :: FunPtr (Ptr HipDeviceptr -> IO ())

peekHipObject :: FunPtr (Ptr a -> IO ()) -> (C2HSImp.ForeignPtr a -> a) -> Ptr (Ptr a) -> IO a
peekHipObject finalizer wrapper ptr = do
  p <- peek ptr
  p <- C2HSImp.newForeignPtr finalizer p
  return $ wrapper p

peekHipModule :: Ptr (Ptr HipModule) -> IO HipModule
peekHipModule = peekHipObject hipModuleUnload HipModule

peekHipStream :: Ptr (Ptr HipStream) -> IO HipStream
peekHipStream = peekHipObject hipStreamDestroy HipStream

peekHipArray :: Ptr (Ptr HipArray) -> IO HipArray
peekHipArray = peekHipObject hipArrayDestroy HipArray

withMaybeHipStream :: Maybe HipStream -> (Ptr HipStream -> IO a) -> IO a
withMaybeHipStream Nothing act = act nullPtr
withMaybeHipStream (Just s) act = withHipStream s act

withParamConfig :: Ptr () -> Int -> (Ptr (Ptr ())  -> IO a) -> IO a
withParamConfig argptr argsize act = with argsize $ \argsizeptr -> do
  let hip_launch_param_buffer_ptr = 1 :: WordPtr
      hip_launch_param_buffer_size = 2 :: WordPtr
      hip_launch_param_end = 3 :: WordPtr
  let comps = [wordPtrToPtr hip_launch_param_buffer_ptr, argptr,
               wordPtrToPtr hip_launch_param_buffer_size, castPtr argsizeptr,
               wordPtrToPtr hip_launch_param_end] :: [Ptr ()]
      bs = toByteString $ mconcat $ map fromStorable comps
  unsafeUseAsCString bs (act . castPtr)

peekDeviceptr :: Ptr (Ptr ()) -> IO HipDeviceptr
peekDeviceptr pptr = do
  p <- peek pptr
  return $ HipDeviceptr $ castPtr p

devicePtrAsRaw :: HipDeviceptr -> Ptr ()
devicePtrAsRaw (HipDeviceptr ptr) = castPtr ptr

unsafeRawAsDeviceptr :: Ptr () -> HipDeviceptr
unsafeRawAsDeviceptr ptr = HipDeviceptr (castPtr ptr)
