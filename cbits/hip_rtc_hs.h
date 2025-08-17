#include "hip/hiprtc.h"

// the raw hiprtcDestroyProgram takes a pointer to hiprtcProgram,
// weird and making it not possible to be used as a finalizer.
hiprtcResult hiprtcDestroyProgram_wrapped(hiprtcProgram prog);
