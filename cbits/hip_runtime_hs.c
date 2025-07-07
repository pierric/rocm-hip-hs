#include "hip_runtime_hs.h"

hipError_t hipLaunchKernel_wrapped(const void *function_address,
                                   dim3 *numBlocks, dim3 *dimBlocks,
                                   void **args,
                                   size_t sharedMemBytes __dparm(0),
                                   hipStream_t stream __dparm(0)) {
  return hipLaunchKernel(function_address, *numBlocks, *dimBlocks, args,
                         sharedMemBytes, stream);
}
