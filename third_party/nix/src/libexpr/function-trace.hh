#pragma once

#include <chrono>

#include "eval.hh"

namespace nix {

struct FunctionCallTrace {
  const Pos& pos;
  FunctionCallTrace(const Pos& pos);
  ~FunctionCallTrace();
};
}  // namespace nix
