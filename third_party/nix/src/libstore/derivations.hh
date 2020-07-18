#pragma once

#include <map>

#include "libproto/worker.pb.h"
#include "libstore/store-api.hh"
#include "libutil/hash.hh"
#include "libutil/types.hh"

namespace nix {

/* Extension of derivations in the Nix store. */
const std::string drvExtension = ".drv";

/* Abstract syntax of derivations. */

struct DerivationOutput {
  Path path;
  std::string hashAlgo; /* hash used for expected hash computation */
  std::string hash;     /* expected hash, may be null */
  DerivationOutput() {}
  // TODO(grfn): Make explicit
  DerivationOutput(Path path, std::string hashAlgo, std::string hash) {
    this->path = path;
    this->hashAlgo = hashAlgo;
    this->hash = hash;
  }

  explicit DerivationOutput(
      const nix::proto::Derivation_DerivationOutput& proto_derivation_output)
      : path(proto_derivation_output.path().path()),
        hashAlgo(proto_derivation_output.hash_algo()),
        hash(proto_derivation_output.hash()) {}

  void parseHashInfo(bool& recursive, Hash& hash) const;
};

// TODO(grfn): change to absl::flat_hash_map
typedef std::map<std::string, DerivationOutput> DerivationOutputs;

/* For inputs that are sub-derivations, we specify exactly which
   output IDs we are interested in. */
// TODO(grfn): change to absl::flat_hash_map
typedef std::map<Path, StringSet> DerivationInputs;

// TODO(grfn): change to absl::flat_hash_map
typedef std::map<std::string, std::string> StringPairs;

struct BasicDerivation {
  DerivationOutputs outputs; /* keyed on symbolic IDs */
  PathSet inputSrcs;         /* inputs that are sources */
  std::string platform;
  Path builder;
  Strings args;
  StringPairs env;

  BasicDerivation(){};

  // Convert the given proto derivation to a BasicDerivation in the given
  // nix::Store.
  static BasicDerivation from_proto(
      const nix::proto::Derivation* proto_derivation, const nix::Store* store);

  virtual ~BasicDerivation(){};

  /* Return the path corresponding to the output identifier `id' in
     the given derivation. */
  Path findOutput(const std::string& id) const;

  bool isBuiltin() const;

  /* Return true iff this is a fixed-output derivation. */
  bool isFixedOutput() const;

  /* Return the output paths of a derivation. */
  PathSet outputPaths() const;
};

struct Derivation : BasicDerivation {
  DerivationInputs inputDrvs; /* inputs that are sub-derivations */

  /* Print a derivation. */
  std::string unparse() const;
};

class Store;

/* Write a derivation to the Nix store, and return its path. */
Path writeDerivation(const ref<Store>& store, const Derivation& drv,
                     const std::string& name, RepairFlag repair = NoRepair);

/* Read a derivation from a file. */
Derivation readDerivation(const Path& drvPath);

/* Check whether a file name ends with the extension for
   derivations. */
bool isDerivation(const std::string& fileName);

Hash hashDerivationModulo(Store& store, Derivation drv);

/* Memoisation of hashDerivationModulo(). */
typedef std::map<Path, Hash> DrvHashes;

extern DrvHashes drvHashes;  // FIXME: global, not thread-safe

/* Split a string specifying a derivation and a set of outputs
   (/nix/store/hash-foo!out1,out2,...) into the derivation path and
   the outputs. */
typedef std::pair<std::string, std::set<std::string> > DrvPathWithOutputs;
DrvPathWithOutputs parseDrvPathWithOutputs(absl::string_view path);

Path makeDrvPathWithOutputs(const Path& drvPath,
                            const std::set<std::string>& outputs);

bool wantOutput(const std::string& output, const std::set<std::string>& wanted);

struct Source;
struct Sink;

Source& readDerivation(Source& in, Store& store, BasicDerivation& drv);
Sink& operator<<(Sink& out, const BasicDerivation& drv);

std::string hashPlaceholder(const std::string& outputName);

}  // namespace nix
