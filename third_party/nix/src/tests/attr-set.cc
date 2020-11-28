#include "libexpr/attr-set.hh"

#include <cstdio>
#include <optional>
#include <string>
#include <vector>

#include <absl/container/btree_map.h>
#include <bits/stdint-intn.h>
#include <gtest/gtest.h>
#include <rapidcheck.h>
#include <rapidcheck/Assertions.h>
#include <rapidcheck/gen/Arbitrary.h>
#include <rapidcheck/gen/Build.h>
#include <rapidcheck/gen/Create.h>
#include <rapidcheck/gen/Transform.h>
#include <rapidcheck/gtest.h>

#include "libexpr/eval.hh"
#include "libexpr/nixexpr.hh"
#include "libexpr/symbol-table.hh"
#include "libexpr/value.hh"
#include "tests/arbitrary.hh"
#include "tests/dummy-store.hh"

namespace nix {

using nix::tests::DummyStore;

class AttrSetTest : public ::testing::Test {
 protected:
  EvalState* eval_state_;
  void SetUp() override {
    nix::expr::InitGC();
    auto store = std::make_shared<DummyStore>();
    eval_state_ = new EvalState({"."}, ref<Store>(store));
    tests::symbol_table = &eval_state_->symbols;
  }

  void assert_bindings_equal(nix::Bindings* lhs, nix::Bindings* rhs) {
    RC_ASSERT(lhs->Equal(rhs, *eval_state_));
  }
};

class AttrSetMonoidTest : public AttrSetTest {};

RC_GTEST_FIXTURE_PROP(AttrSetMonoidTest, mergeLeftIdentity,
                      (nix::Bindings && bindings)) {
  auto empty_bindings = nix::Bindings::New();
  auto result = Bindings::Merge(*empty_bindings, bindings);
  assert_bindings_equal(result.get(), &bindings);
}

RC_GTEST_FIXTURE_PROP(AttrSetMonoidTest, mergeRightIdentity,
                      (nix::Bindings && bindings)) {
  auto empty_bindings = nix::Bindings::New();
  auto result = Bindings::Merge(bindings, *empty_bindings);
  assert_bindings_equal(result.get(), &bindings);
}

RC_GTEST_FIXTURE_PROP(AttrSetMonoidTest, mergeAssociative,
                      (nix::Bindings && bindings_1, nix::Bindings&& bindings_2,
                       nix::Bindings&& bindings_3)) {
  auto b231 =
      Bindings::Merge(bindings_1, *Bindings::Merge(bindings_2, bindings_3));
  auto b123 =
      Bindings::Merge(*Bindings::Merge(bindings_1, bindings_2), bindings_3);
  assert_bindings_equal(b231.get(), b123.get());
}

}  // namespace nix
