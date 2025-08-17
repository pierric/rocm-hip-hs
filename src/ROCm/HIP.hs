{-# LANGUAGE TemplateHaskell #-}

module ROCm.HIP
  ( hipGetDeviceCount,
    hipGetDevice,
    hipSetDevice,
    hipDeviceSynchronize,
    hipMalloc,
    hipMallocAsync,
    hipFree,
    hipMemcpy,
    hipMemcpyHtoD,
    hipMemcpyDtoH,
    hipMemcpyDtoD,
    hipMemcpyWithStream,
    hipMemsetD8,
    hipMemsetD8Async,
    hipMemsetD16,
    hipMemsetD16Async,
    hipMemsetD32,
    hipMemsetD32Async,
    hipMemGetInfo,
    hipMemPtrGetInfo,
    hipArrayCreate,
    hipStreamCreate,
    hipStreamSynchronize,
    hipModuleLoad,
    hipModuleGetFunction,
    hipModuleLaunchKernel,
    hipLaunchKernel,
    Runtime.HipModule,
    Runtime.HipFunction,
    Runtime.HipError (..),
    Runtime.HipArrayFormat (..),
    Runtime.HipArrayDescriptor (..),
    Runtime.HipMemcpyKind (..),
    hiprtcVersion,
    hiprtcCreateProgram,
    hiprtcCompileProgram,
    hiprtcGetProgramLog,
    withHipDeviceMem,
    Runtime.devicePtrAsRaw,
    Runtime.hipFreeAsFunPtr,
    Runtime.advanceDeviceptr,
    HipDeviceptr (..),
    Dim3 (..),
    KernelArg (..),
    RTC.HiprtcProgram,
    RTC.HiprtcResult (..),
  )
where

import Blaze.ByteString.Builder (fromStorable, fromWord8, toByteString)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int
import Foreign.C.String (peekCStringLen)
import Foreign.C.Types (CSize)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable
import qualified ROCm.HIP.RTC as RTC
import ROCm.HIP.Runtime (Dim3 (..), HipDeviceptr)
import qualified ROCm.HIP.Runtime as Runtime
import ROCm.HIP.TH
import ROCm.HIP.Utils (withCStringList)

$(checked 'Runtime.hipGetDeviceCount)
$(checked 'Runtime.hipGetDevice)
$(checked 'Runtime.hipSetDevice)
$(checked 'Runtime.hipDeviceSynchronize)

$(checked 'Runtime.hipMalloc)
$(checked 'Runtime.hipMallocAsync)
$(checked 'Runtime.hipFree)

withHipDeviceMem :: CSize -> (Runtime.HipDeviceptr -> IO a) -> IO a
withHipDeviceMem size = bracket (hipMalloc size) (hipFree)

$(checked 'Runtime.hipMemcpy)
$(checked 'Runtime.hipMemcpyWithStream)
$(checked 'Runtime.hipMemcpyHtoD)
$(checked 'Runtime.hipMemcpyDtoH)
$(checked 'Runtime.hipMemcpyDtoD)
$(checked 'Runtime.hipMemsetD8)
$(checked 'Runtime.hipMemsetD8Async)
$(checked 'Runtime.hipMemsetD16)
$(checked 'Runtime.hipMemsetD16Async)
$(checked 'Runtime.hipMemsetD32)
$(checked 'Runtime.hipMemsetD32Async)
$(checked 'Runtime.hipMemGetInfo)
$(checked 'Runtime.hipMemPtrGetInfo)

$(checked 'Runtime.hipArrayCreate)

$(checked 'Runtime.hipStreamCreate)
$(checked 'Runtime.hipStreamSynchronize)

$(checked 'Runtime.hipModuleLoad)
$(checked 'Runtime.hipModuleGetFunction)
$(checked 'Runtime.hipModuleLaunchKernelRaw)

$(checked 'Runtime.hipLaunchKernel)

$(checked 'RTC.hiprtcVersion)

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

hipModuleLaunchKernel ::
  Runtime.HipFunction ->
  Dim3 ->
  Dim3 ->
  Int ->
  Maybe Runtime.HipStream ->
  [KernelArg] ->
  IO ()
hipModuleLaunchKernel func grid block shm stream args = do
  let Dim3 gridX gridY gridZ = grid
      Dim3 blockX blockY blockZ = block
  unsafeUseAsCStringLen (pack args) $ \(ptr, len) -> do
    Runtime.withParamConfig (castPtr ptr) len $ \param -> do
      hipModuleLaunchKernelRaw func gridX gridY gridZ blockX blockY blockZ (fromIntegral shm) stream nullPtr param

hiprtcCreateProgram :: String -> String -> [(String, String)] -> IO RTC.HiprtcProgram
hiprtcCreateProgram name code inc = do
  let inc_num = fromIntegral $ length inc
  let (inc_names, inc_headers) = unzip inc
  withCStringList inc_names $ \pn ->
    withCStringList inc_headers $ \ph -> do
      (rc, rt) <- RTC.hiprtcCreateProgram code name inc_num ph pn
      if (rc == toEnum 0) then return rt else error ("HIP error: " ++ show rc)

hiprtcCompileProgram :: RTC.HiprtcProgram -> [String] -> IO RTC.HiprtcResult
hiprtcCompileProgram prog options = do
  let opt_num = fromIntegral $ length options
  withCStringList options $ \po -> do
    RTC.hiprtcCompileProgram prog opt_num po

hiprtcGetProgramLog :: RTC.HiprtcProgram -> IO String
hiprtcGetProgramLog prog = do
  ret <- RTC.hiprtcGetProgramLogSize prog
  case ret of
    (rc, _) | rc /= toEnum 0 -> error ("HIP error: " ++ show rc)
    (_, 0) -> return ""
    (_, s) -> do
      let size = fromIntegral s
      bracket (mallocBytes size) free $ \buf -> do
        rc <- RTC.hiprtcGetProgramLog prog buf
        if (rc /= toEnum 0)
          then error ("HIP error: " ++ show rc)
          else peekCStringLen (buf, size)
