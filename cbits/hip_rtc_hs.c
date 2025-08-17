#include "hip_rtc_hs.h"

hiprtcResult hiprtcDestroyProgram_wrapped(hiprtcProgram prog) {
  return hiprtcDestroyProgram(&prog);
}
