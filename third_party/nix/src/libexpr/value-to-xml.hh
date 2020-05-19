#pragma once

#include <map>
#include <string>

#include "eval.hh"
#include "nixexpr.hh"

namespace nix {

void printValueAsXML(EvalState& state, bool strict, bool location, Value& v,
                     std::ostream& out, PathSet& context);

}
