#include "libstore/derivations.hh"

#include <memory>

#include <absl/strings/str_cat.h>
#include <gtest/gtest.h>
#include <rapidcheck.h>
#include <rapidcheck/Assertions.h>
#include <rapidcheck/gen/Arbitrary.h>
#include <rapidcheck/gen/Build.h>
#include <rapidcheck/gen/Container.h>
#include <rapidcheck/gen/Tuple.h>
#include <rapidcheck/gtest.h>
#include <rapidcheck/state.h>

#include "libexpr/eval.hh"
#include "libutil/hash.hh"
#include "libutil/types.hh"
#include "tests/arbitrary.hh"

namespace nix {

void AssertDerivationsEqual(const Derivation& lhs, const Derivation& rhs) {
  RC_ASSERT(lhs.outputs.size() == rhs.outputs.size());
  for (const auto& [k, lhs_v] : lhs.outputs) {
    auto rhs_v = rhs.outputs.find(k);
    RC_ASSERT(rhs_v != rhs.outputs.end());
    RC_ASSERT(lhs_v.path == rhs_v->second.path);
    RC_ASSERT(lhs_v.hashAlgo == rhs_v->second.hashAlgo);
    RC_ASSERT(lhs_v.hash == rhs_v->second.hash);
  }

  RC_ASSERT(lhs.inputSrcs == rhs.inputSrcs);
  RC_ASSERT(lhs.platform == rhs.platform);
  RC_ASSERT(lhs.builder == rhs.builder);
  RC_ASSERT(lhs.args == rhs.args);
  RC_ASSERT(lhs.env == rhs.env);
  RC_ASSERT(lhs.inputDrvs == rhs.inputDrvs);
}

class DerivationsTest : public ::testing::Test {};

// NOLINTNEXTLINE
RC_GTEST_FIXTURE_PROP(DerivationsTest, UnparseParseRoundTrip,
                      (Derivation && drv)) {
  auto unparsed = drv.unparse();
  auto parsed = parseDerivation(unparsed);
  AssertDerivationsEqual(drv, parsed);
}

// NOLINTNEXTLINE
RC_GTEST_FIXTURE_PROP(DerivationsTest, ToProtoPreservesInput,
                      (Derivation && drv)) {
  auto proto = drv.to_proto();

  RC_ASSERT(proto.outputs_size() == drv.outputs.size());
  RC_ASSERT(proto.input_sources().paths_size() == drv.inputSrcs.size());
  auto paths = proto.input_sources().paths();
  for (const auto& input_src : drv.inputSrcs) {
    RC_ASSERT(std::find(paths.begin(), paths.end(), input_src) != paths.end());
  }

  RC_ASSERT(proto.platform() == drv.platform);
  RC_ASSERT(proto.builder().path() == drv.builder);

  RC_ASSERT(proto.args_size() == drv.args.size());
  auto args = proto.args();
  for (const auto& arg : drv.args) {
    RC_ASSERT(std::find(args.begin(), args.end(), arg) != args.end());
  }

  RC_ASSERT(proto.env_size() == drv.env.size());
  auto env = proto.env();
  for (const auto& [key, value] : drv.env) {
    RC_ASSERT(env.at(key) == value);
  }
}

class ParseDrvPathWithOutputsTest : public DerivationsTest {};

TEST(ParseDrvPathWithOutputsTest, ParseDrvPathWithOutputs) {
  auto input = "/nix/store/my51f75kp056md84gq2v08pd140pcz57-test.drv!out";
  auto result = nix::parseDrvPathWithOutputs(input);

  ASSERT_EQ(result.first,
            "/nix/store/my51f75kp056md84gq2v08pd140pcz57-test.drv");
  ASSERT_EQ(result.second, nix::PathSet{"out"});
}

TEST(ParseDrvPathWithOutputsTest, ParseDrvPathWithMultipleOutputs) {
  auto input = "/nix/store/my51f75kp056md84gq2v08pd140pcz57-test.drv!out,dev";
  auto result = nix::parseDrvPathWithOutputs(input);

  nix::PathSet expected = {"out", "dev"};

  ASSERT_EQ(result.first,
            "/nix/store/my51f75kp056md84gq2v08pd140pcz57-test.drv");
  ASSERT_EQ(result.second, expected);
}

TEST(ParseDrvPathWithOutputsTest, ParseDrvPathWithNoOutputs) {
  auto input = "/nix/store/my51f75kp056md84gq2v08pd140pcz57-test";
  auto result = nix::parseDrvPathWithOutputs(input);

  ASSERT_EQ(result.first, "/nix/store/my51f75kp056md84gq2v08pd140pcz57-test");
  ASSERT_EQ(result.second, nix::PathSet());
}

}  // namespace nix
