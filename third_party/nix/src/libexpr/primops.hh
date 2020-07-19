#include <tuple>
#include <vector>

#include "libexpr/eval.hh"

namespace nix {

struct RegisterPrimOp {
  typedef std::vector<std::tuple<std::string, size_t, PrimOpFun>> PrimOps;
  static PrimOps* primOps;
  /* You can register a constant by passing an arity of 0. fun
     will get called during EvalState initialization, so there
     may be primops not yet added and builtins is not yet sorted. */
  RegisterPrimOp(const std::string& name, size_t arity, PrimOpFun fun);
};

/* These primops are disabled without enableNativeCode */
/* Load a ValueInitializer from a DSO and return whatever it initializes */
void prim_importNative(EvalState& state, const Pos& pos, Value** args,
                       Value& v);

}  // namespace nix
