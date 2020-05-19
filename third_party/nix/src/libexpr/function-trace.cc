#include "function-trace.hh"
#include <glog/logging.h>

namespace nix {

FunctionCallTrace::FunctionCallTrace(const Pos& pos) : pos(pos) {
  auto duration = std::chrono::high_resolution_clock::now().time_since_epoch();
  auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(duration);
  LOG(INFO) << "function-trace entered " << pos << " at " << ns.count();
}

FunctionCallTrace::~FunctionCallTrace() {
  auto duration = std::chrono::high_resolution_clock::now().time_since_epoch();
  auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(duration);
  LOG(INFO) << "function-trace exited " << pos << " at " << ns.count();
}

}  // namespace nix
