#pragma once

#include <map>
#include <string>

#include "libexpr/eval.hh"

namespace nix {

Value* findAlongAttrPath(EvalState& state, const std::string& attrPath,
                         Bindings& autoArgs, Value& vIn);

}
