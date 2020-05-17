#pragma once

#include <map>
#include <string>
#include "eval.hh"
#include "nixexpr.hh"

namespace nix {

class JSONPlaceholder;

void printValueAsJSON(EvalState& state, bool strict, Value& v,
                      JSONPlaceholder& out, PathSet& context);

void printValueAsJSON(EvalState& state, bool strict, Value& v,
                      std::ostream& str, PathSet& context);

}  // namespace nix
