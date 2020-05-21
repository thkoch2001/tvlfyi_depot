#pragma once

#include <string>

#include "eval.hh"

namespace nix {

MakeError(JSONParseError, EvalError)

    void parseJSON(EvalState& state, const std::string& s, Value& v);

}
