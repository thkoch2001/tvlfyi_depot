#pragma once

#include <chrono>

#include "libexpr/eval.hh"

namespace nix {

struct FunctionCallTrace {
  const Pos& pos;
  FunctionCallTrace(const Pos& pos);
  ~FunctionCallTrace();
};
}  // namespace nix
