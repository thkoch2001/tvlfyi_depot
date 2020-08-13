#pragma once

#include "libutil/args.hh"

namespace nix {

class Store;
class EvalState;
class Bindings;

struct MixEvalArgs : virtual Args {
  MixEvalArgs();

  std::unique_ptr<Bindings> getAutoArgs(EvalState& state);

  Strings searchPath;

 private:
  std::map<std::string, std::string> autoArgs;
};

Path lookupFileArg(EvalState& state, std::string s);

}  // namespace nix
