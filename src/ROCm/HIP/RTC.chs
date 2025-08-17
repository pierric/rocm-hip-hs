#include "hip/hiprtc.h"

module ROCm.HIP.RTC where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, Storable(..))

import ROCm.HIP.Utils (peekHipObject)

{#typedef size_t CSize#}

{#enum hiprtcResult as HiprtcResult {upcaseFirstLetter} deriving (Eq, Show)#}

{#pointer hiprtcProgram as HiprtcProgram foreign newtype#}

{#fun hiprtcGetErrorString as ^
  {`HiprtcResult'} -> `String'#} 

{#fun hiprtcVersion as ^
  {alloca- `CInt' peek*, alloca- `CInt' peek*} -> `HiprtcResult' #}

{#fun hiprtcCreateProgram as ^
  {
    alloca- `HiprtcProgram' peekHiprtcProgram*,
    `String',
    `String',
    `CInt',
    id `Ptr (Ptr CChar)',
    id `Ptr (Ptr CChar)'
  } -> `HiprtcResult' #}

foreign import ccall safe "Internal.chs.h &hiprtcDestroyProgram_wrapped"
  hiprtcDestroyProgram :: FunPtr (Ptr HiprtcProgram -> IO ())

{#fun hiprtcCompileProgram as ^
  {
    `HiprtcProgram',
    `CInt',
    id `Ptr (Ptr CChar)'
  } -> `HiprtcResult' #}

{#fun hiprtcGetProgramLogSize as ^
 {
    `HiprtcProgram',
    alloca- `CSize' peek*
 } -> `HiprtcResult' #}

{#fun hiprtcGetProgramLog as ^
 {
    `HiprtcProgram',
    id `Ptr CChar'
 } -> `HiprtcResult' #}

peekHiprtcProgram :: Ptr (Ptr HiprtcProgram) -> IO HiprtcProgram
peekHiprtcProgram = peekHipObject hiprtcDestroyProgram HiprtcProgram

-- hiprtcGetCode
-- hiprtcGetCodeSize
-- hiprtcGetBitcodeSize
