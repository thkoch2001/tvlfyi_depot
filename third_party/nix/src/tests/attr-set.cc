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
#include "tests/dummy-store.hh"

static nix::SymbolTable* symbol_table;

namespace rc {
using nix::Pos;
using nix::Value;

// TODO(grfn): These arbitrary implementations should be pulled out to a util
// file sooner rather than later

template <>
struct Arbitrary<nix::Symbol> {
  static Gen<nix::Symbol> arbitrary() {
    return gen::map(gen::arbitrary<std::string>(),
                    [](std::string s) { return symbol_table->Create(s); });
  }
};

template <>
struct Arbitrary<Value> {
  static Gen<nix::Value> arbitrary() {
    return gen::build(gen::construct<Value>(),
                      // TODO(grfn) generalize to more types
                      gen::set(&Value::type, gen::just(nix::ValueType::tInt)),
                      gen::set(&Value::integer, gen::arbitrary<int64_t>()));
  }
};

template <>
struct Arbitrary<Value*> {
  static Gen<nix::Value*> arbitrary() {
    return gen::apply(
        [](nix::ValueType typ, int i) {
          auto ret = new Value();
          ret->type = typ;
          ret->integer = i;
          return ret;
        },
        gen::just(nix::ValueType::tInt), gen::arbitrary<int64_t>());
  }
};

template <>
struct Arbitrary<nix::Pos> {
  static Gen<nix::Pos> arbitrary() {
    return gen::construct<nix::Pos>(gen::arbitrary<nix::Symbol>(),
                                    gen::arbitrary<unsigned int>(),
                                    gen::arbitrary<unsigned int>());
  }
};

template <>
struct Arbitrary<nix::Pos*> {
  static Gen<nix::Pos*> arbitrary() {
    return gen::apply(
        [](unsigned int line, unsigned int column) {
          return new Pos({}, line, column);
        },
        gen::arbitrary<unsigned int>(), gen::arbitrary<unsigned int>());
  }
};

template <>
struct Arbitrary<nix::Attr> {
  static Gen<nix::Attr> arbitrary() {
    return gen::construct<nix::Attr>(gen::arbitrary<nix::Symbol>(),
                                     gen::arbitrary<Value*>(),
                                     gen::arbitrary<nix::Pos*>());
  }
};

template <>
struct Arbitrary<nix::Bindings> {
  static Gen<nix::Bindings> arbitrary() {
    return gen::map(gen::arbitrary<std::vector<nix::Attr>>(), [](auto attrs) {
      nix::Bindings res;
      for (const auto& attr : attrs) {
        res.push_back(attr);
      }
      return res;
    });
  }
};

}  // namespace rc

namespace nix {

using nix::tests::DummyStore;

class AttrSetTest : public ::testing::Test {
 protected:
  EvalState* eval_state_;
  void SetUp() override {
    nix::expr::InitGC();
    auto store = std::make_shared<DummyStore>();
    eval_state_ = new EvalState({"."}, ref<Store>(store));
    symbol_table = &eval_state_->symbols;
  }

  void assert_bindings_equal(nix::Bindings& lhs, nix::Bindings& rhs) {
    Value lhs_val;
    Value rhs_val;
    lhs_val.type = rhs_val.type = ValueType::tAttrs;
    lhs_val.attrs = &lhs;
    rhs_val.attrs = &lhs;

    RC_ASSERT(eval_state_->eqValues(lhs_val, rhs_val));
  }
};

class AttrSetMonoidTest : public AttrSetTest {};

RC_GTEST_FIXTURE_PROP(AttrSetMonoidTest, mergeLeftIdentity,
                      (nix::Bindings && bindings)) {
  auto empty_bindings = nix::Bindings::NewGC();
  auto result = *Bindings::Merge(*empty_bindings, bindings);
  assert_bindings_equal(result, bindings);
}

RC_GTEST_FIXTURE_PROP(AttrSetMonoidTest, mergeRightIdentity,
                      (nix::Bindings && bindings)) {
  auto empty_bindings = nix::Bindings::NewGC();
  auto result = *Bindings::Merge(bindings, *empty_bindings);
  assert_bindings_equal(result, bindings);
}

RC_GTEST_FIXTURE_PROP(AttrSetMonoidTest, mergeAssociative,
                      (nix::Bindings && bindings_1, nix::Bindings&& bindings_2,
                       nix::Bindings&& bindings_3)) {
  assert_bindings_equal(
      *Bindings::Merge(bindings_1, *Bindings::Merge(bindings_2, bindings_3)),
      *Bindings::Merge(*Bindings::Merge(bindings_1, bindings_2), bindings_3));
}

}  // namespace nix
