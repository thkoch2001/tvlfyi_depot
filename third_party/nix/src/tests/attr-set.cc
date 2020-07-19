#include "libexpr/attr-set.hh"

#include <cstdio>
#include <optional>
#include <string>
#include <vector>

#include <bits/stdint-intn.h>
#include <gc/gc_cpp.h>
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

static nix::SymbolTable symbol_table;

namespace rc {
using nix::Pos;
using nix::Value;

template <>
struct Arbitrary<nix::Symbol> {
  static Gen<nix::Symbol> arbitrary() {
    return gen::map(gen::arbitrary<std::string>(),
                    [](std::string s) { return symbol_table.Create(s); });
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
          auto ret = new (GC) Value();
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
          return new (GC) Pos({}, line, column);
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

class AttrSetTest : public ::testing::Test {
 protected:
  static void SetUpTestCase() { nix::initGC(); }
  static void TearDownTestCase() {}
};

class AttrSetMonoidTest : public AttrSetTest {};

void assert_bindings_equal(nix::Bindings& lhs, nix::Bindings& rhs) {
  RC_ASSERT(lhs.size() == rhs.size());

  for (const auto& lhs_val : lhs) {
    const auto& rhs_val = rhs.find(lhs_val.first);
    RC_ASSERT(rhs_val != rhs.end());
    RC_ASSERT(*rhs_val->second.pos == *lhs_val.second.pos);
    RC_ASSERT(rhs_val->second.value->type == lhs_val.second.value->type);
    RC_ASSERT(rhs_val->second.value->integer == lhs_val.second.value->integer);
  }
}

RC_GTEST_PROP(AttrSetMonoidTest, mergeLeftIdentity, (nix::Bindings && bindings)) {
  auto empty_bindings = nix::Bindings::NewGC();
  auto result = *Bindings::Merge(*empty_bindings, bindings);
  assert_bindings_equal(result, bindings);
}

RC_GTEST_PROP(AttrSetMonoidTest, mergeRightIdentity,
              (nix::Bindings && bindings)) {
  auto empty_bindings = nix::Bindings::NewGC();
  auto result = *Bindings::Merge(bindings, *empty_bindings);
  assert_bindings_equal(result, bindings);
}

RC_GTEST_PROP(AttrSetMonoidTest, mergeAssociative,
              (nix::Bindings && bindings_1, nix::Bindings&& bindings_2,
               nix::Bindings&& bindings_3)) {
  assert_bindings_equal(
      *Bindings::Merge(bindings_1, *Bindings::Merge(bindings_2, bindings_3)),
      *Bindings::Merge(*Bindings::Merge(bindings_1, bindings_2), bindings_3));
}

}  // namespace nix
