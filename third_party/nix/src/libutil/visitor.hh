#pragma once

namespace nix::util {

// Helper class used for visiting std::variants by creating a variadic
// list of lambda expressions that delegates calls to each of the
// callables.
//
// See e.g.
// https://dev.to/tmr232/that-overloaded-trick-overloading-lambdas-in-c17
template <class... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};

template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

}  // namespace nix::util
