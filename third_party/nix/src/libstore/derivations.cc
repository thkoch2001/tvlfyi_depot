#include "libstore/derivations.hh"

#include <absl/strings/match.h>
#include <absl/strings/str_split.h>
#include <absl/strings/string_view.h>
#include <glog/logging.h>

#include "libproto/worker.pb.h"
#include "libstore/fs-accessor.hh"
#include "libstore/globals.hh"
#include "libstore/store-api.hh"
#include "libstore/worker-protocol.hh"
#include "libutil/istringstream_nocopy.hh"
#include "libutil/util.hh"

namespace nix {

// TODO(#statusor): looks like easy absl::Status conversion
void DerivationOutput::parseHashInfo(bool& recursive, Hash& hash) const {
  recursive = false;
  std::string algo = hashAlgo;

  if (std::string(algo, 0, 2) == "r:") {
    recursive = true;
    algo = std::string(algo, 2);
  }

  HashType hashType = parseHashType(algo);
  if (hashType == htUnknown) {
    throw Error(format("unknown hash algorithm '%1%'") % algo);
  }

  auto hash_ = Hash::deserialize(this->hash, hashType);
  hash = Hash::unwrap_throw(hash_);
}

nix::proto::Derivation_DerivationOutput DerivationOutput::to_proto() const {
  nix::proto::Derivation_DerivationOutput result;
  result.mutable_path()->set_path(path);
  result.set_hash_algo(hashAlgo);
  result.set_hash(hash);
  return result;
}

BasicDerivation BasicDerivation::from_proto(
    const nix::proto::Derivation* proto_derivation, const nix::Store& store) {
  BasicDerivation result;
  result.platform = proto_derivation->platform();
  result.builder = proto_derivation->builder().path();
  store.assertStorePath(result.builder);

  for (auto [k, v] : proto_derivation->outputs()) {
    result.outputs.emplace(k, v);
  }

  result.inputSrcs.insert(proto_derivation->input_sources().paths().begin(),
                          proto_derivation->input_sources().paths().end());

  result.args.insert(result.args.end(), proto_derivation->args().begin(),
                     proto_derivation->args().end());

  for (auto [k, v] : proto_derivation->env()) {
    result.env.emplace(k, v);
  }

  return result;
}
nix::proto::Derivation BasicDerivation::to_proto() const {
  nix::proto::Derivation result;
  to_proto(&result);
  return result;
}

void BasicDerivation::to_proto(nix::proto::Derivation* result) const {
  for (const auto& [key, output] : outputs) {
    result->mutable_outputs()->insert({key, output.to_proto()});
  }
  for (const auto& input_src : inputSrcs) {
    result->mutable_input_sources()->add_paths(input_src);
  }
  result->set_platform(platform);
  result->mutable_builder()->set_path(builder);
  for (const auto& arg : args) {
    result->add_args(arg);
  }

  for (const auto& [key, value] : env) {
    result->mutable_env()->insert({key, value});
  }
}

Path BasicDerivation::findOutput(const std::string& id) const {
  auto i = outputs.find(id);
  if (i == outputs.end()) {
    throw Error(format("derivation has no output '%1%'") % id);
  }
  return i->second.path;
}

bool BasicDerivation::isBuiltin() const {
  return std::string(builder, 0, 8) == "builtin:";
}

Path writeDerivation(const ref<Store>& store, const Derivation& drv,
                     const std::string& name, RepairFlag repair) {
  PathSet references;
  references.insert(drv.inputSrcs.begin(), drv.inputSrcs.end());
  for (auto& i : drv.inputDrvs) {
    references.insert(i.first);
  }
  /* Note that the outputs of a derivation are *not* references
     (that can be missing (of course) and should not necessarily be
     held during a garbage collection). */
  std::string suffix = name + drvExtension;
  std::string contents = drv.unparse();
  return settings.readOnlyMode
             ? store->computeStorePathForText(suffix, contents, references)
             : store->addTextToStore(suffix, contents, references, repair);
}

/* Read string `s' from stream `str'. */
static void expect(std::istream& str, const std::string& s) {
  char s2[s.size()];
  str.read(s2, s.size());
  if (std::string(s2, s.size()) != s) {
    throw FormatError(format("expected string '%1%'") % s);
  }
}

/* Read a C-style string from stream `str'. */
static std::string parseString(std::istream& str) {
  std::string res;
  expect(str, "\"");
  int c;
  while ((c = str.get()) != '"' && c != EOF) {
    if (c == '\\') {
      c = str.get();
      if (c == 'n') {
        res += '\n';
      } else if (c == 'r') {
        res += '\r';
      } else if (c == 't') {
        res += '\t';
      } else if (c == EOF) {
        throw FormatError("unexpected EOF while parsing C-style escape");
      } else {
        res += static_cast<char>(c);
      }
    } else {
      res += static_cast<char>(c);
    }
  }
  return res;
}

static Path parsePath(std::istream& str) {
  std::string s = parseString(str);
  if (s.empty() || s[0] != '/') {
    throw FormatError(format("bad path '%1%' in derivation") % s);
  }
  return s;
}

static bool endOfList(std::istream& str) {
  if (str.peek() == ',') {
    str.get();
    return false;
  }
  if (str.peek() == ']') {
    str.get();
    return true;
  }
  return false;
}

static StringSet parseStrings(std::istream& str, bool arePaths) {
  StringSet res;
  while (!endOfList(str)) {
    res.insert(arePaths ? parsePath(str) : parseString(str));
  }
  return res;
}

Derivation parseDerivation(const std::string& s) {
  Derivation drv;
  istringstream_nocopy str(s);
  expect(str, "Derive([");

  /* Parse the list of outputs. */
  while (!endOfList(str)) {
    DerivationOutput out;
    expect(str, "(");
    std::string id = parseString(str);
    expect(str, ",");
    out.path = parsePath(str);
    expect(str, ",");
    out.hashAlgo = parseString(str);
    expect(str, ",");
    out.hash = parseString(str);
    expect(str, ")");
    drv.outputs[id] = out;
  }

  /* Parse the list of input derivations. */
  expect(str, ",[");
  while (!endOfList(str)) {
    expect(str, "(");
    Path drvPath = parsePath(str);
    expect(str, ",[");
    drv.inputDrvs[drvPath] = parseStrings(str, false);
    expect(str, ")");
  }

  expect(str, ",[");
  drv.inputSrcs = parseStrings(str, true);
  expect(str, ",");
  drv.platform = parseString(str);
  expect(str, ",");
  drv.builder = parseString(str);

  /* Parse the builder arguments. */
  expect(str, ",[");
  while (!endOfList(str)) {
    drv.args.push_back(parseString(str));
  }

  /* Parse the environment variables. */
  expect(str, ",[");
  while (!endOfList(str)) {
    expect(str, "(");
    std::string name = parseString(str);
    expect(str, ",");
    std::string value = parseString(str);
    expect(str, ")");
    drv.env[name] = value;
  }

  expect(str, ")");
  return drv;
}

Derivation readDerivation(const Path& drvPath) {
  try {
    return parseDerivation(readFile(drvPath));
  } catch (FormatError& e) {
    throw Error(format("error parsing derivation '%1%': %2%") % drvPath %
                e.msg());
  }
}

Derivation Store::derivationFromPath(const Path& drvPath) {
  assertStorePath(drvPath);
  ensurePath(drvPath);
  auto accessor = getFSAccessor();
  try {
    return parseDerivation(accessor->readFile(drvPath));
  } catch (FormatError& e) {
    throw Error(format("error parsing derivation '%1%': %2%") % drvPath %
                e.msg());
  }
}

const char* findChunk(const char* begin) {
  while (*begin != 0 && *begin != '\"' && *begin != '\\' && *begin != '\n' &&
         *begin != '\r' && *begin != '\t') {
    begin++;
  }

  return begin;
}

static void printString(std::string& res, const std::string& s) {
  res += '"';

  const char* it = s.c_str();
  while (*it != 0) {
    const char* end = findChunk(it);
    std::copy(it, end, std::back_inserter(res));

    it = end;

    switch (*it) {
      case '"':
      case '\\':
        res += "\\";
        res += *it;
        break;
      case '\n':
        res += "\\n";
        break;
      case '\r':
        res += "\\r";
        break;
      case '\t':
        res += "\\t";
        break;
      default:
        continue;
    }

    it++;
  }

  res += '"';
}

template <class ForwardIterator>
static void printStrings(std::string& res, ForwardIterator i,
                         ForwardIterator j) {
  res += '[';
  bool first = true;
  for (; i != j; ++i) {
    if (first) {
      first = false;
    } else {
      res += ',';
    }
    printString(res, *i);
  }
  res += ']';
}

std::string Derivation::unparse() const {
  std::string s;
  s.reserve(65536);
  s += "Derive([";

  bool first = true;
  for (auto& i : outputs) {
    if (first) {
      first = false;
    } else {
      s += ',';
    }
    s += '(';
    printString(s, i.first);
    s += ',';
    printString(s, i.second.path);
    s += ',';
    printString(s, i.second.hashAlgo);
    s += ',';
    printString(s, i.second.hash);
    s += ')';
  }

  s += "],[";
  first = true;
  for (auto& i : inputDrvs) {
    if (first) {
      first = false;
    } else {
      s += ',';
    }
    s += '(';
    printString(s, i.first);
    s += ',';
    printStrings(s, i.second.begin(), i.second.end());
    s += ')';
  }

  s += "],";
  printStrings(s, inputSrcs.begin(), inputSrcs.end());

  s += ',';
  printString(s, platform);
  s += ',';
  printString(s, builder);
  s += ',';
  printStrings(s, args.begin(), args.end());

  s += ",[";
  first = true;
  for (auto& i : env) {
    if (first) {
      first = false;
    } else {
      s += ',';
    }
    s += '(';
    printString(s, i.first);
    s += ',';
    printString(s, i.second);
    s += ')';
  }

  s += "])";

  return s;
}

bool isDerivation(const std::string& fileName) {
  return absl::EndsWith(fileName, drvExtension);
}

bool BasicDerivation::isFixedOutput() const {
  return outputs.size() == 1 && outputs.begin()->first == "out" &&
         !outputs.begin()->second.hash.empty();
}

DrvHashes drvHashes;

/* Returns the hash of a derivation modulo fixed-output
   subderivations.  A fixed-output derivation is a derivation with one
   output (`out') for which an expected hash and hash algorithm are
   specified (using the `outputHash' and `outputHashAlgo'
   attributes).  We don't want changes to such derivations to
   propagate upwards through the dependency graph, changing output
   paths everywhere.

   For instance, if we change the url in a call to the `fetchurl'
   function, we do not want to rebuild everything depending on it
   (after all, (the hash of) the file being downloaded is unchanged).
   So the *output paths* should not change.  On the other hand, the
   *derivation paths* should change to reflect the new dependency
   graph.

   That's what this function does: it returns a hash which is just the
   hash of the derivation ATerm, except that any input derivation
   paths have been replaced by the result of a recursive call to this
   function, and that for fixed-output derivations we return a hash of
   its output path. */
Hash hashDerivationModulo(Store& store, Derivation drv) {
  /* Return a fixed hash for fixed-output derivations. */
  if (drv.isFixedOutput()) {
    auto i = drv.outputs.begin();
    return hashString(htSHA256, "fixed:out:" + i->second.hashAlgo + ":" +
                                    i->second.hash + ":" + i->second.path);
  }

  /* For other derivations, replace the inputs paths with recursive
     calls to this function.*/
  DerivationInputs inputs2;
  for (auto& i : drv.inputDrvs) {
    Hash h = drvHashes[i.first];
    if (!h) {
      assert(store.isValidPath(i.first));
      Derivation drv2 = readDerivation(store.toRealPath(i.first));
      h = hashDerivationModulo(store, drv2);
      drvHashes[i.first] = h;
    }
    inputs2[h.to_string(Base16, false)] = i.second;
  }
  drv.inputDrvs = inputs2;

  return hashString(htSHA256, drv.unparse());
}

DrvPathWithOutputs parseDrvPathWithOutputs(absl::string_view path) {
  auto pos = path.find('!');
  if (pos == absl::string_view::npos) {
    return DrvPathWithOutputs(path, std::set<std::string>());
  }

  return DrvPathWithOutputs(
      path.substr(0, pos),
      absl::StrSplit(path.substr(pos + 1), absl::ByChar(','),
                     absl::SkipEmpty()));
}

Path makeDrvPathWithOutputs(const Path& drvPath,
                            const std::set<std::string>& outputs) {
  return outputs.empty() ? drvPath
                         : drvPath + "!" + concatStringsSep(",", outputs);
}

bool wantOutput(const std::string& output,
                const std::set<std::string>& wanted) {
  return wanted.empty() || wanted.find(output) != wanted.end();
}

PathSet BasicDerivation::outputPaths() const {
  PathSet paths;
  for (auto& i : outputs) {
    paths.insert(i.second.path);
  }
  return paths;
}

Source& readDerivation(Source& in, Store& store, BasicDerivation& drv) {
  drv.outputs.clear();
  auto nr = readNum<size_t>(in);
  for (size_t n = 0; n < nr; n++) {
    auto name = readString(in);
    DerivationOutput o;
    in >> o.path >> o.hashAlgo >> o.hash;
    store.assertStorePath(o.path);
    drv.outputs[name] = o;
  }

  drv.inputSrcs = readStorePaths<PathSet>(store, in);
  in >> drv.platform >> drv.builder;
  drv.args = readStrings<Strings>(in);

  nr = readNum<size_t>(in);
  for (size_t n = 0; n < nr; n++) {
    auto key = readString(in);
    auto value = readString(in);
    drv.env[key] = value;
  }

  return in;
}

Sink& operator<<(Sink& out, const BasicDerivation& drv) {
  out << drv.outputs.size();
  for (auto& i : drv.outputs) {
    out << i.first << i.second.path << i.second.hashAlgo << i.second.hash;
  }
  out << drv.inputSrcs << drv.platform << drv.builder << drv.args;
  out << drv.env.size();
  for (auto& i : drv.env) {
    out << i.first << i.second;
  }
  return out;
}

std::string hashPlaceholder(const std::string& outputName) {
  // FIXME: memoize?
  return "/" + hashString(htSHA256, "nix-output:" + outputName)
                   .to_string(Base32, false);
}

}  // namespace nix
