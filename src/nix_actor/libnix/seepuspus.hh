#pragma once
#include "eval.hh"

namespace nix {

  ref<EvalState> newEvalState(ref<Store> store)
  {
    auto searchPath = Strings();
    auto evalState =
      #if HAVE_BOEHMGC
        std::allocate_shared<EvalState>(
          traceable_allocator<EvalState>(), searchPath, store, store)
      #else
        std::make_shared<EvalState>(
          searchPath, store, store)
      #endif
        ;
    return ref<EvalState>(evalState);
  }

}
