#include <hip/hiprtc.h>
#include <hip/hip_runtime.h>

extern "C" __global__ void saxpy(float a, float *x, float *y, int n) {
  const unsigned int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) {
    y[i] = a * x[i] + y[i];
  }
}
