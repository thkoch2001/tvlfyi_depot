#pragma once

#include <map>
#include <string>

#include "libexpr/eval.hh"
#include "libexpr/nixexpr.hh"

namespace nix {

class JSONPlaceholder;

void printValueAsJSON(EvalState& state, bool strict, Value& v,
                      JSONPlaceholder& out, PathSet& context);

void printValueAsJSON(EvalState& state, bool strict, Value& v,
                      std::ostream& str, PathSet& context);

}  // namespace nix
