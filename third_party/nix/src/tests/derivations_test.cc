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

namespace rc {

using nix::Derivation;
using nix::DerivationOutput;

template <>
struct Arbitrary<nix::Base> {
  static Gen<nix::Base> arbitrary() {
    return gen::element(nix::Base16, nix::Base32, nix::Base64);
  }
};

template <>
struct Arbitrary<DerivationOutput> {
  static Gen<DerivationOutput> arbitrary() {
    return gen::apply(
        [](std::string content, std::string path, std::string hash_algo,
           bool recursive, bool include_algo_in_hash, nix::Base base) {
          auto hash_type = nix::parseHashType(hash_algo);
          auto hash = nix::hashString(hash_type, content);
          return DerivationOutput(
              path, recursive ? absl::StrCat("r:", hash_algo) : hash_algo,
              hash.to_string(base, include_algo_in_hash));
        },
        gen::arbitrary<std::string>(),
        gen::map(gen::arbitrary<std::string>(),
                 [](std::string s) { return absl::StrCat("/", s); }),
        gen::element<std::string>("md5", "sha1", "sha256", "sha512"),
        gen::arbitrary<bool>(), gen::arbitrary<bool>(),
        gen::arbitrary<nix::Base>());
  }
};

template <>
struct Arbitrary<Derivation> {
  static Gen<Derivation> arbitrary() {
    auto gen_path = gen::map(gen::arbitrary<std::string>(), [](std::string s) {
      return absl::StrCat("/", s);
    });

    return gen::build<Derivation>(
        gen::set(&nix::BasicDerivation::outputs),
        gen::set(&nix::BasicDerivation::inputSrcs,
                 gen::container<nix::PathSet>(gen_path)),
        gen::set(&nix::BasicDerivation::platform),
        gen::set(&nix::BasicDerivation::builder, gen_path),
        gen::set(&nix::BasicDerivation::args),
        gen::set(&nix::BasicDerivation::env),
        gen::set(&Derivation::inputDrvs,
                 gen::container<nix::DerivationInputs>(
                     gen_path, gen::arbitrary<nix::StringSet>())));
  }
};

}  // namespace rc

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

class DerivationsTest : public ::testing::Test {
 protected:
};

// NOLINTNEXTLINE
RC_GTEST_FIXTURE_PROP(DerivationsTest, UnparseParseRoundTrip,
                      (Derivation && drv)) {
  auto unparsed = drv.unparse();
  auto parsed = parseDerivation(unparsed);
  AssertDerivationsEqual(drv, parsed);
}

}  // namespace nix
