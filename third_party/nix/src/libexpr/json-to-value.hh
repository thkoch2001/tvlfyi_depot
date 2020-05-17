#pragma once

#include <string>
#include "eval.hh"

namespace nix {

MakeError(JSONParseError, EvalError)

    void parseJSON(EvalState& state, const string& s, Value& v);

}
