#pragma once

#include <rapidcheck.h>
#include <rapidcheck/Gen.h>
#include <rapidcheck/gen/Arbitrary.h>

#include "libexpr/attr-set.hh"
#include "libexpr/nixexpr.hh"
#include "libstore/derivations.hh"
#include "libutil/hash.hh"

namespace nix::tests {
static nix::SymbolTable* symbol_table;
}

namespace rc {

using nix::Derivation;
using nix::DerivationOutput;
using nix::Pos;
using nix::Value;

template <>
struct Arbitrary<nix::Symbol> {
  static Gen<nix::Symbol> arbitrary() {
    return gen::map(gen::arbitrary<std::string>(), [](std::string s) {
      return nix::tests::symbol_table->Create(s);
    });
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

template <class K, class V>
struct Arbitrary<absl::btree_map<K, V>> {
  static Gen<absl::btree_map<K, V>> arbitrary() {
    return gen::map(gen::arbitrary<std::map<K, V>>(), [](std::map<K, V> map) {
      absl::btree_map<K, V> out_map;
      out_map.insert(map.begin(), map.end());
      return out_map;
    });
  }
};

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

template <>
struct Arbitrary<nix::BuildResult::Status> {
  static Gen<nix::BuildResult::Status> arbitrary() {
    return gen::element(nix::BuildResult::Status::Built,
                        nix::BuildResult::Status::Substituted,
                        nix::BuildResult::Status::AlreadyValid,
                        nix::BuildResult::Status::PermanentFailure,
                        nix::BuildResult::Status::InputRejected,
                        nix::BuildResult::Status::OutputRejected,
                        nix::BuildResult::Status::TransientFailure,
                        nix::BuildResult::Status::CachedFailure,
                        nix::BuildResult::Status::TimedOut,
                        nix::BuildResult::Status::MiscFailure,
                        nix::BuildResult::Status::DependencyFailed,
                        nix::BuildResult::Status::LogLimitExceeded,
                        nix::BuildResult::Status::NotDeterministic);
  }
};
}  // namespace rc
