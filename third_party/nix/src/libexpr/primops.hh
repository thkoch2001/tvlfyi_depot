#include <tuple>
#include <vector>

#include "libexpr/eval.hh"

namespace nix {

struct RegisterPrimOp {
  using PrimOps = std::vector<std::tuple<std::string, size_t, PrimOpFun> >;
  static PrimOps* primOps;
  /* You can register a constant by passing an arity of 0. fun
     will get called during EvalState initialization, so there
     may be primops not yet added and builtins is not yet sorted. */
  RegisterPrimOp(const std::string& name, size_t arity, PrimOpFun fun);
};

}  // namespace nix
