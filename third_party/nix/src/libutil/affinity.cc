#include "affinity.hh"

#include <glog/logging.h>

#include "types.hh"
#include "util.hh"

#if __linux__
#include <sched.h>
#endif

namespace nix {

#if __linux__
static bool didSaveAffinity = false;
static cpu_set_t savedAffinity;
#endif

void setAffinityTo(int cpu) {
#if __linux__
  if (sched_getaffinity(0, sizeof(cpu_set_t), &savedAffinity) == -1) {
    return;
  }

  didSaveAffinity = true;
  DLOG(INFO) << "locking this thread to CPU " << cpu;
  cpu_set_t newAffinity;
  CPU_ZERO(&newAffinity);
  CPU_SET(cpu, &newAffinity);
  if (sched_setaffinity(0, sizeof(cpu_set_t), &newAffinity) == -1) {
    LOG(ERROR) << "failed to lock thread to CPU " << cpu;
  }
#endif
}

int lockToCurrentCPU() {
#if __linux__
  int cpu = sched_getcpu();
  if (cpu != -1) {
    setAffinityTo(cpu);
  }
  return cpu;
#else
  return -1;
#endif
}

void restoreAffinity() {
#if __linux__
  if (!didSaveAffinity) {
    return;
  }

  if (sched_setaffinity(0, sizeof(cpu_set_t), &savedAffinity) == -1) {
    LOG(ERROR) << "failed to restore affinity";
  }
#endif
}

}  // namespace nix
