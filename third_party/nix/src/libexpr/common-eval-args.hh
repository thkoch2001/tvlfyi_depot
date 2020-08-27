#pragma once

#include "libutil/args.hh"

namespace nix {

class Store;
class EvalState;
class Bindings;

enum ArgType { kArgTypeString, kArgTypeExpr };

struct MixEvalArgs : virtual Args {
  MixEvalArgs();

  std::unique_ptr<Bindings> getAutoArgs(EvalState& state);

  Strings searchPath;

 private:
  std::map<std::string, std::pair<ArgType, std::string>> auto_args_;
};

Path lookupFileArg(EvalState& state, std::string s);

}  // namespace nix
