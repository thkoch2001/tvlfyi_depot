#pragma once

#include <map>
#include <string>

#include "eval.hh"

namespace nix {

Value* findAlongAttrPath(EvalState& state, const string& attrPath,
                         Bindings& autoArgs, Value& vIn);

}
