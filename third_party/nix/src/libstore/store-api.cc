#include "libstore/store-api.hh"

#include <future>
#include <ostream>
#include <streambuf>
#include <utility>

#include <absl/status/status.h>
#include <absl/strings/match.h>
#include <absl/strings/numbers.h>
#include <absl/strings/str_cat.h>
#include <absl/strings/str_split.h>
#include <glog/logging.h>
#include <grpcpp/create_channel.h>

#include "libproto/worker.pb.h"
#include "libstore/crypto.hh"
#include "libstore/derivations.hh"
#include "libstore/globals.hh"
#include "libstore/nar-info-disk-cache.hh"
#include "libstore/rpc-store.hh"
#include "libutil/json.hh"
#include "libutil/thread-pool.hh"
#include "libutil/util.hh"

namespace nix {

namespace {
class NullStream : public std::streambuf {
 public:
  int overflow(int c) override { return c; }
};

static NullStream NULL_STREAM{};
}  // namespace

std::ostream DiscardLogsSink() { return std::ostream(&NULL_STREAM); }

std::optional<BuildMode> BuildModeFrom(nix::proto::BuildMode mode) {
  switch (mode) {
    case nix::proto::BuildMode::Normal:
      return BuildMode::bmNormal;
    case nix::proto::BuildMode::Repair:
      return BuildMode::bmRepair;
    case nix::proto::BuildMode::Check:
      return BuildMode::bmCheck;
    default:
      return {};
  }
}

nix::proto::BuildMode BuildModeToProto(BuildMode mode) {
  switch (mode) {
    case BuildMode::bmNormal:
      return nix::proto::BuildMode::Normal;
    case BuildMode::bmRepair:
      return nix::proto::BuildMode::Repair;
    case BuildMode::bmCheck:
      return nix::proto::BuildMode::Check;
  }
}

nix::proto::BuildStatus BuildResult::status_to_proto() {
  switch (status) {
    case BuildResult::Status::Built:
      return proto::BuildStatus::Built;
    case BuildResult::Status::Substituted:
      return proto::BuildStatus::Substituted;
    case BuildResult::Status::AlreadyValid:
      return proto::BuildStatus::AlreadyValid;
    case BuildResult::Status::PermanentFailure:
      return proto::BuildStatus::PermanentFailure;
    case BuildResult::Status::InputRejected:
      return proto::BuildStatus::InputRejected;
    case BuildResult::Status::OutputRejected:
      return proto::BuildStatus::OutputRejected;
    case BuildResult::Status::TransientFailure:
      return proto::BuildStatus::TransientFailure;
    case BuildResult::Status::CachedFailure:
      return proto::BuildStatus::CachedFailure;
    case BuildResult::Status::TimedOut:
      return proto::BuildStatus::TimedOut;
    case BuildResult::Status::MiscFailure:
      return proto::BuildStatus::MiscFailure;
    case BuildResult::Status::DependencyFailed:
      return proto::BuildStatus::DependencyFailed;
    case BuildResult::Status::LogLimitExceeded:
      return proto::BuildStatus::LogLimitExceeded;
    case BuildResult::Status::NotDeterministic:
      return proto::BuildStatus::NotDeterministic;
  }
}

std::optional<BuildResult> BuildResult::FromProto(
    const nix::proto::BuildResult& resp) {
  BuildResult result;
  switch (resp.status()) {
    case proto::BuildStatus::Built:
      result.status = BuildResult::Status::Built;
      break;
    case proto::BuildStatus::Substituted:
      result.status = BuildResult::Status::Substituted;
      break;
    case proto::BuildStatus::AlreadyValid:
      result.status = BuildResult::Status::AlreadyValid;
      break;
    case proto::BuildStatus::PermanentFailure:
      result.status = BuildResult::Status::PermanentFailure;
      break;
    case proto::BuildStatus::InputRejected:
      result.status = BuildResult::Status::InputRejected;
      break;
    case proto::BuildStatus::OutputRejected:
      result.status = BuildResult::Status::OutputRejected;
      break;
    case proto::BuildStatus::TransientFailure:
      result.status = BuildResult::Status::TransientFailure;
      break;
    case proto::BuildStatus::CachedFailure:
      result.status = BuildResult::Status::CachedFailure;
      break;
    case proto::BuildStatus::TimedOut:
      result.status = BuildResult::Status::TimedOut;
      break;
    case proto::BuildStatus::MiscFailure:
      result.status = BuildResult::Status::MiscFailure;
      break;
    case proto::BuildStatus::DependencyFailed:
      result.status = BuildResult::Status::DependencyFailed;
      break;
    case proto::BuildStatus::LogLimitExceeded:
      result.status = BuildResult::Status::LogLimitExceeded;
      break;
    case proto::BuildStatus::NotDeterministic:
      result.status = BuildResult::Status::NotDeterministic;
      break;
    default:
      return {};
  }

  result.errorMsg = resp.msg();
  return result;
}

std::optional<GCOptions::GCAction> GCActionFromProto(
    nix::proto::GCAction gc_action) {
  switch (gc_action) {
    case nix::proto::GCAction::ReturnLive:
      return GCOptions::GCAction::gcReturnLive;
    case nix::proto::GCAction::ReturnDead:
      return GCOptions::GCAction::gcReturnDead;
    case nix::proto::GCAction::DeleteDead:
      return GCOptions::GCAction::gcDeleteDead;
    case nix::proto::GCAction::DeleteSpecific:
      return GCOptions::GCAction::gcDeleteSpecific;
    default:
      return {};
  }
}

[[nodiscard]] const proto::GCAction GCOptions::ActionToProto() const {
  switch (action) {
    case GCOptions::GCAction::gcReturnLive:
      return nix::proto::GCAction::ReturnLive;
    case GCOptions::GCAction::gcReturnDead:
      return nix::proto::GCAction::ReturnDead;
    case GCOptions::GCAction::gcDeleteDead:
      return nix::proto::GCAction::DeleteDead;
    case GCOptions::GCAction::gcDeleteSpecific:
      return nix::proto::GCAction::DeleteSpecific;
  }
}

bool Store::isInStore(const Path& path) const {
  return isInDir(path, storeDir);
}

bool Store::isStorePath(const Path& path) const {
  return isInStore(path) &&
         path.size() >= storeDir.size() + 1 + storePathHashLen &&
         path.find('/', storeDir.size() + 1) == Path::npos;
}

void Store::assertStorePath(const Path& path) const {
  if (!isStorePath(path)) {
    throw Error(format("path '%1%' is not in the Nix store") % path);
  }
}

Path Store::toStorePath(const Path& path) const {
  if (!isInStore(path)) {
    throw Error(format("path '%1%' is not in the Nix store") % path);
  }
  Path::size_type slash = path.find('/', storeDir.size() + 1);
  if (slash == Path::npos) {
    return path;
  }
  return Path(path, 0, slash);
}

Path Store::followLinksToStore(const Path& _path) const {
  Path path = absPath(_path);
  while (!isInStore(path)) {
    if (!isLink(path)) {
      break;
    }
    std::string target = readLink(path);
    path = absPath(target, dirOf(path));
  }
  if (!isInStore(path)) {
    throw Error(format("path '%1%' is not in the Nix store") % path);
  }
  return path;
}

Path Store::followLinksToStorePath(const Path& path) const {
  return toStorePath(followLinksToStore(path));
}

std::string storePathToName(const Path& path) {
  auto base = baseNameOf(path);

  // The base name of the store path must be `storePathHashLen` characters long,
  // if it is not `storePathHashLen` long then the next character, following
  // the hash part, MUST be a dash (`-`).
  const bool hasLengthMismatch = base.size() != storePathHashLen;
  const bool hasInvalidSuffix =
      base.size() > storePathHashLen && base[storePathHashLen] != '-';
  if (hasLengthMismatch && hasInvalidSuffix) {
    throw Error(format("path '%1%' is not a valid store path") % path);
  }

  return base.size() == storePathHashLen
             ? ""
             : std::string(base, storePathHashLen + 1);
}

std::string storePathToHash(const Path& path) {
  auto base = baseNameOf(path);
  assert(base.size() >= storePathHashLen);
  return std::string(base, 0, storePathHashLen);
}

void checkStoreName(const std::string& name) {
  std::string validChars = "+-._?=";

  auto baseError =
      format(
          "The path name '%2%' is invalid: %3%. "
          "Path names are alphanumeric and can include the symbols %1% "
          "and must not begin with a period. "
          "Note: If '%2%' is a source file and you cannot rename it on "
          "disk, builtins.path { name = ... } can be used to give it an "
          "alternative name.") %
      validChars % name;

  /* Disallow names starting with a dot for possible security
     reasons (e.g., "." and ".."). */
  if (std::string(name, 0, 1) == ".") {
    throw Error(baseError % "it is illegal to start the name with a period");
  }
  /* Disallow names longer than 211 characters. ext4’s max is 256,
     but we need extra space for the hash and .chroot extensions. */
  if (name.length() > 211) {
    throw Error(baseError % "name must be less than 212 characters");
  }
  for (auto& i : name) {
    if (!((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z') ||
          (i >= '0' && i <= '9') || validChars.find(i) != std::string::npos)) {
      throw Error(baseError % (format("the '%1%' character is invalid") % i));
    }
  }
}

/* Store paths have the following form:

   <store>/<h>-<name>

   where

   <store> = the location of the Nix store, usually /nix/store

   <name> = a human readable name for the path, typically obtained
     from the name attribute of the derivation, or the name of the
     source file from which the store path is created.  For derivation
     outputs other than the default "out" output, the string "-<id>"
     is suffixed to <name>.

   <h> = base-32 representation of the first 160 bits of a SHA-256
     hash of <s>; the hash part of the store name

   <s> = the string "<type>:sha256:<h2>:<store>:<name>";
     note that it includes the location of the store as well as the
     name to make sure that changes to either of those are reflected
     in the hash (e.g. you won't get /nix/store/<h>-name1 and
     /nix/store/<h>-name2 with equal hash parts).

   <type> = one of:
     "text:<r1>:<r2>:...<rN>"
       for plain text files written to the store using
       addTextToStore(); <r1> ... <rN> are the references of the
       path.
     "source"
       for paths copied to the store using addToStore() when recursive
       = true and hashAlgo = "sha256"
     "output:<id>"
       for either the outputs created by derivations, OR paths copied
       to the store using addToStore() with recursive != true or
       hashAlgo != "sha256" (in that case "source" is used; it's
       silly, but it's done that way for compatibility).  <id> is the
       name of the output (usually, "out").

   <h2> = base-16 representation of a SHA-256 hash of:
     if <type> = "text:...":
       the string written to the resulting store path
     if <type> = "source":
       the serialisation of the path from which this store path is
       copied, as returned by hashPath()
     if <type> = "output:<id>":
       for non-fixed derivation outputs:
         the derivation (see hashDerivationModulo() in
         primops.cc)
       for paths copied by addToStore() or produced by fixed-output
       derivations:
         the string "fixed:out:<rec><algo>:<hash>:", where
           <rec> = "r:" for recursive (path) hashes, or "" for flat
             (file) hashes
           <algo> = "md5", "sha1" or "sha256"
           <hash> = base-16 representation of the path or flat hash of
             the contents of the path (or expected contents of the
             path for fixed-output derivations)

   It would have been nicer to handle fixed-output derivations under
   "source", e.g. have something like "source:<rec><algo>", but we're
   stuck with this for now...

   The main reason for this way of computing names is to prevent name
   collisions (for security).  For instance, it shouldn't be feasible
   to come up with a derivation whose output path collides with the
   path for a copied source.  The former would have a <s> starting with
   "output:out:", while the latter would have a <s> starting with
   "source:".
*/

Path Store::makeStorePath(const std::string& type, const Hash& hash,
                          const std::string& name) const {
  /* e.g., "source:sha256:1abc...:/nix/store:foo.tar.gz" */
  std::string s =
      type + ":" + hash.to_string(Base16) + ":" + storeDir + ":" + name;

  checkStoreName(name);

  return absl::StrCat(storeDir, "/", hashString(htSHA256, s).ToStorePathHash(),
                      "-", name);
}

Path Store::makeOutputPath(const std::string& id, const Hash& hash,
                           const std::string& name) const {
  return makeStorePath("output:" + id, hash,
                       name + (id == "out" ? "" : "-" + id));
}

Path Store::makeFixedOutputPath(bool recursive, const Hash& hash,
                                const std::string& name) const {
  return hash.type == htSHA256 && recursive
             ? makeStorePath("source", hash, name)
             : makeStorePath(
                   "output:out",
                   hashString(
                       htSHA256,
                       absl::StrCat("fixed:out:", (recursive ? "r:" : ""),
                                    hash.to_string(Base16), ":")),
                   name);
}

Path Store::makeTextPath(const std::string& name, const Hash& hash,
                         const PathSet& references) const {
  assert(hash.type == htSHA256);
  /* Stuff the references (if any) into the type.  This is a bit
     hacky, but we can't put them in `s' since that would be
     ambiguous. */
  std::string type = "text";
  for (auto& i : references) {
    type += ":";
    type += i;
  }
  return makeStorePath(type, hash, name);
}

std::pair<Path, Hash> Store::computeStorePathForPath(const std::string& name,
                                                     const Path& srcPath,
                                                     bool recursive,
                                                     HashType hashAlgo,
                                                     PathFilter& filter) const {
  Hash h = recursive ? hashPath(hashAlgo, srcPath, filter).first
                     : hashFile(hashAlgo, srcPath);
  Path dstPath = makeFixedOutputPath(recursive, h, name);
  return std::pair<Path, Hash>(dstPath, h);
}

Path Store::computeStorePathForText(const std::string& name,
                                    const std::string& s,
                                    const PathSet& references) const {
  return makeTextPath(name, hashString(htSHA256, s), references);
}

Store::Store(const Params& params)
    : Config(params),
      state(Sync<State>{
          State{LRUCache<std::string, std::shared_ptr<ValidPathInfo>>(
              static_cast<size_t>(pathInfoCacheSize))}}) {}

std::string Store::getUri() { return ""; }

bool Store::isValidPath(const Path& storePath) {
  assertStorePath(storePath);

  auto hashPart = storePathToHash(storePath);

  {
    auto state_(state.lock());
    auto res = state_->pathInfoCache.get(hashPart);
    if (res) {
      stats.narInfoReadAverted++;
      return *res != nullptr;
    }
  }

  if (diskCache) {
    auto res = diskCache->lookupNarInfo(getUri(), hashPart);
    if (res.first != NarInfoDiskCache::oUnknown) {
      stats.narInfoReadAverted++;
      auto state_(state.lock());
      state_->pathInfoCache.upsert(
          hashPart,
          res.first == NarInfoDiskCache::oInvalid ? nullptr : res.second);
      return res.first == NarInfoDiskCache::oValid;
    }
  }

  bool valid = isValidPathUncached(storePath);

  if (diskCache && !valid) {
    // FIXME: handle valid = true case.
    diskCache->upsertNarInfo(getUri(), hashPart, nullptr);
  }

  return valid;
}

/* Default implementation for stores that only implement
   queryPathInfoUncached(). */
bool Store::isValidPathUncached(const Path& path) {
  try {
    queryPathInfo(path);
    return true;
  } catch (InvalidPath&) {
    return false;
  }
}

ref<const ValidPathInfo> Store::queryPathInfo(const Path& storePath) {
  std::promise<ref<ValidPathInfo>> promise;

  queryPathInfo(
      storePath,
      Callback<ref<ValidPathInfo>>([&](std::future<ref<ValidPathInfo>> result) {
        try {
          promise.set_value(result.get());
        } catch (...) {
          promise.set_exception(std::current_exception());
        }
      }));

  return promise.get_future().get();
}

void Store::queryPathInfo(const Path& storePath,
                          Callback<ref<ValidPathInfo>> callback) noexcept {
  std::string hashPart;

  try {
    assertStorePath(storePath);

    hashPart = storePathToHash(storePath);

    {
      auto res = state.lock()->pathInfoCache.get(hashPart);
      if (res) {
        stats.narInfoReadAverted++;
        if (!*res) {
          throw InvalidPath(format("path '%s' is not valid") % storePath);
        }
        return callback(ref<ValidPathInfo>(*res));
      }
    }

    if (diskCache) {
      auto res = diskCache->lookupNarInfo(getUri(), hashPart);
      if (res.first != NarInfoDiskCache::oUnknown) {
        stats.narInfoReadAverted++;
        {
          auto state_(state.lock());
          state_->pathInfoCache.upsert(
              hashPart,
              res.first == NarInfoDiskCache::oInvalid ? nullptr : res.second);
          if (res.first == NarInfoDiskCache::oInvalid ||
              (res.second->path != storePath &&
               !storePathToName(storePath).empty())) {
            throw InvalidPath(format("path '%s' is not valid") % storePath);
          }
        }
        return callback(ref<ValidPathInfo>(res.second));
      }
    }

  } catch (...) {
    return callback.rethrow();
  }

  auto callbackPtr = std::make_shared<decltype(callback)>(std::move(callback));

  queryPathInfoUncached(
      storePath,
      Callback<std::shared_ptr<ValidPathInfo>>{
          [this, storePath, hashPart,
           callbackPtr](std::future<std::shared_ptr<ValidPathInfo>> fut) {
            try {
              auto info = fut.get();

              if (diskCache) {
                diskCache->upsertNarInfo(getUri(), hashPart, info);
              }

              {
                auto state_(state.lock());
                state_->pathInfoCache.upsert(hashPart, info);
              }

              if (!info || (info->path != storePath &&
                            !storePathToName(storePath).empty())) {
                stats.narInfoMissing++;
                throw InvalidPath("path '%s' is not valid", storePath);
              }

              (*callbackPtr)(ref<ValidPathInfo>(info));
            } catch (...) {
              callbackPtr->rethrow();
            }
          }});
}

PathSet Store::queryValidPaths(const PathSet& paths,
                               SubstituteFlag maybeSubstitute) {
  struct State {
    size_t left;
    PathSet valid;
    std::exception_ptr exc;
  };

  Sync<State> state_(State{paths.size(), PathSet()});

  std::condition_variable wakeup;
  ThreadPool pool;

  auto doQuery = [&](const Path& path) {
    checkInterrupt();
    queryPathInfo(path, Callback<ref<ValidPathInfo>>(
                            [path, &state_,
                             &wakeup](std::future<ref<ValidPathInfo>> fut) {
                              auto state(state_.lock());
                              try {
                                auto info = fut.get();
                                state->valid.insert(path);
                              } catch (InvalidPath&) {
                              } catch (...) {
                                state->exc = std::current_exception();
                              }
                              assert(state->left);
                              if (--state->left == 0u) {
                                wakeup.notify_one();
                              }
                            }));
  };

  for (auto& path : paths) {
    pool.enqueue(std::bind(doQuery, path));
  }

  pool.process();

  while (true) {
    auto state(state_.lock());
    if (state->left == 0u) {
      if (state->exc) {
        std::rethrow_exception(state->exc);
      }
      return state->valid;
    }
    state.wait(wakeup);
  }
}

/* Return a string accepted by decodeValidPathInfo() that
   registers the specified paths as valid.  Note: it's the
   responsibility of the caller to provide a closure. */
std::string Store::makeValidityRegistration(const PathSet& paths,
                                            bool showDerivers, bool showHash) {
  std::string s;

  for (auto& i : paths) {
    s += i + "\n";

    auto info = queryPathInfo(i);

    if (showHash) {
      s += info->narHash.to_string(Base16, false) + "\n";
      s += (format("%1%\n") % info->narSize).str();
    }

    Path deriver = showDerivers ? info->deriver : "";
    s += deriver + "\n";

    s += (format("%1%\n") % info->references.size()).str();

    for (auto& j : info->references) {
      s += j + "\n";
    }
  }

  return s;
}

void Store::pathInfoToJSON(JSONPlaceholder& jsonOut, const PathSet& storePaths,
                           bool includeImpureInfo, bool showClosureSize,
                           AllowInvalidFlag allowInvalid) {
  auto jsonList = jsonOut.list();

  for (auto storePath : storePaths) {
    auto jsonPath = jsonList.object();
    jsonPath.attr("path", storePath);

    try {
      auto info = queryPathInfo(storePath);
      storePath = info->path;

      jsonPath.attr("narHash", info->narHash.to_string())
          .attr("narSize", info->narSize);

      {
        auto jsonRefs = jsonPath.list("references");
        for (auto& ref : info->references) {
          jsonRefs.elem(ref);
        }
      }

      if (!info->ca.empty()) {
        jsonPath.attr("ca", info->ca);
      }

      std::pair<uint64_t, uint64_t> closureSizes;

      if (showClosureSize) {
        closureSizes = getClosureSize(storePath);
        jsonPath.attr("closureSize", closureSizes.first);
      }

      if (includeImpureInfo) {
        if (!info->deriver.empty()) {
          jsonPath.attr("deriver", info->deriver);
        }

        if (info->registrationTime != 0) {
          jsonPath.attr("registrationTime", info->registrationTime);
        }

        if (info->ultimate) {
          jsonPath.attr("ultimate", info->ultimate);
        }

        if (!info->sigs.empty()) {
          auto jsonSigs = jsonPath.list("signatures");
          for (auto& sig : info->sigs) {
            jsonSigs.elem(sig);
          }
        }

        auto narInfo = std::dynamic_pointer_cast<const NarInfo>(
            std::shared_ptr<const ValidPathInfo>(info));

        if (narInfo) {
          if (!narInfo->url.empty()) {
            jsonPath.attr("url", narInfo->url);
          }
          if (narInfo->fileHash) {
            jsonPath.attr("downloadHash", narInfo->fileHash.to_string());
          }
          if (narInfo->fileSize != 0u) {
            jsonPath.attr("downloadSize", narInfo->fileSize);
          }
          if (showClosureSize) {
            jsonPath.attr("closureDownloadSize", closureSizes.second);
          }
        }
      }

    } catch (InvalidPath&) {
      jsonPath.attr("valid", false);
    }
  }
}

std::pair<uint64_t, uint64_t> Store::getClosureSize(const Path& storePath) {
  uint64_t totalNarSize = 0;
  uint64_t totalDownloadSize = 0;
  PathSet closure;
  computeFSClosure(storePath, closure, false, false);
  for (auto& p : closure) {
    auto info = queryPathInfo(p);
    totalNarSize += info->narSize;
    auto narInfo = std::dynamic_pointer_cast<const NarInfo>(
        std::shared_ptr<const ValidPathInfo>(info));
    if (narInfo) {
      totalDownloadSize += narInfo->fileSize;
    }
  }
  return {totalNarSize, totalDownloadSize};
}

const Store::Stats& Store::getStats() {
  {
    auto state_(state.lock());
    stats.pathInfoCacheSize = state_->pathInfoCache.size();
  }
  return stats;
}

absl::Status Store::buildPaths(std::ostream& /* log_sink */,
                               const PathSet& paths, BuildMode) {
  for (auto& path : paths) {
    if (isDerivation(path)) {
      return absl::Status(absl::StatusCode::kUnimplemented,
                          "buildPaths is unsupported");
    }
  }

  if (queryValidPaths(paths).size() != paths.size()) {
    return absl::Status(absl::StatusCode::kUnimplemented,
                        "buildPaths is unsupported");
  }

  return absl::OkStatus();
}

void copyStorePath(ref<Store> srcStore, const ref<Store>& dstStore,
                   const Path& storePath, RepairFlag repair,
                   CheckSigsFlag checkSigs) {
  auto srcUri = srcStore->getUri();
  auto dstUri = dstStore->getUri();

  if (srcUri == "local" || srcUri == "daemon") {
    LOG(INFO) << "copying path '" << storePath << "' to '" << dstUri << "'";
  } else {
    if (dstUri == "local" || dstUri == "daemon") {
      LOG(INFO) << "copying path '" << storePath << "' from '" << srcUri << "'";
    } else {
      LOG(INFO) << "copying path '" << storePath << "' from '" << srcUri
                << "' to '" << dstUri << "'";
    }
  }

  auto info = srcStore->queryPathInfo(storePath);

  uint64_t total = 0;

  if (!info->narHash) {
    StringSink sink;
    srcStore->narFromPath({storePath}, sink);
    auto info2 = make_ref<ValidPathInfo>(*info);
    info2->narHash = hashString(htSHA256, *sink.s);
    if (info->narSize == 0u) {
      info2->narSize = sink.s->size();
    }
    if (info->ultimate) {
      info2->ultimate = false;
    }
    info = info2;

    StringSource source(*sink.s);
    dstStore->addToStore(*info, source, repair, checkSigs);
    return;
  }

  if (info->ultimate) {
    auto info2 = make_ref<ValidPathInfo>(*info);
    info2->ultimate = false;
    info = info2;
  }

  auto source = sinkToSource(
      [&](Sink& sink) {
        LambdaSink wrapperSink([&](const unsigned char* data, size_t len) {
          sink(data, len);
          total += len;
        });
        srcStore->narFromPath({storePath}, wrapperSink);
      },
      [&]() {
        throw EndOfFile("NAR for '%s' fetched from '%s' is incomplete",
                        storePath, srcStore->getUri());
      });

  dstStore->addToStore(*info, *source, repair, checkSigs);
}

void copyPaths(ref<Store> srcStore, ref<Store> dstStore,
               const PathSet& storePaths, RepairFlag repair,
               CheckSigsFlag checkSigs, SubstituteFlag substitute) {
  PathSet valid = dstStore->queryValidPaths(storePaths, substitute);

  PathSet missing;
  for (auto& path : storePaths) {
    if (valid.count(path) == 0u) {
      missing.insert(path);
    }
  }

  if (missing.empty()) {
    return;
  }

  LOG(INFO) << "copying " << missing.size() << " paths";

  std::atomic<size_t> nrDone{0};
  std::atomic<size_t> nrFailed{0};
  std::atomic<uint64_t> bytesExpected{0};
  std::atomic<uint64_t> nrRunning{0};

  ThreadPool pool;

  processGraph<Path>(
      pool, PathSet(missing.begin(), missing.end()),

      [&](const Path& storePath) {
        if (dstStore->isValidPath(storePath)) {
          nrDone++;
          return PathSet();
        }

        auto info = srcStore->queryPathInfo(storePath);

        bytesExpected += info->narSize;

        return info->references;
      },

      [&](const Path& storePath) {
        checkInterrupt();

        if (!dstStore->isValidPath(storePath)) {
          MaintainCount<decltype(nrRunning)> mc(nrRunning);
          try {
            copyStorePath(srcStore, dstStore, storePath, repair, checkSigs);
          } catch (Error& e) {
            nrFailed++;
            if (!settings.keepGoing) {
              throw e;
            }
            LOG(ERROR) << "could not copy " << storePath << ": " << e.what();
            return;
          }
        }

        nrDone++;
      });
}

void copyClosure(const ref<Store>& srcStore, const ref<Store>& dstStore,
                 const PathSet& storePaths, RepairFlag repair,
                 CheckSigsFlag checkSigs, SubstituteFlag substitute) {
  PathSet closure;
  srcStore->computeFSClosure({storePaths}, closure);
  copyPaths(srcStore, dstStore, closure, repair, checkSigs, substitute);
}

ValidPathInfo decodeValidPathInfo(std::istream& str, bool hashGiven) {
  ValidPathInfo info;
  getline(str, info.path);
  if (str.eof()) {
    info.path = "";
    return info;
  }
  if (hashGiven) {
    std::string s;
    getline(str, s);
    auto hash_ = Hash::deserialize(s, htSHA256);
    info.narHash = Hash::unwrap_throw(hash_);
    getline(str, s);
    if (!absl::SimpleAtoi(s, &info.narSize)) {
      throw Error("number expected");
    }
  }
  getline(str, info.deriver);
  std::string s;
  int n;
  getline(str, s);
  if (!absl::SimpleAtoi(s, &n)) {
    throw Error("number expected");
  }
  while ((n--) != 0) {
    getline(str, s);
    info.references.insert(s);
  }
  if (!str || str.eof()) {
    throw Error("missing input");
  }
  return info;
}

std::string showPaths(const PathSet& paths) {
  std::string s;
  for (auto& i : paths) {
    if (!s.empty()) {
      s += ", ";
    }
    s += "'" + i + "'";
  }
  return s;
}

std::string ValidPathInfo::fingerprint() const {
  if (narSize == 0 || !narHash) {
    throw Error(format("cannot calculate fingerprint of path '%s' because its "
                       "size/hash is not known") %
                path);
  }
  return "1;" + path + ";" + narHash.to_string(Base32) + ";" +
         std::to_string(narSize) + ";" + concatStringsSep(",", references);
}

void ValidPathInfo::sign(const SecretKey& secretKey) {
  sigs.insert(secretKey.signDetached(fingerprint()));
}

bool ValidPathInfo::isContentAddressed(const Store& store) const {
  auto warn = [&]() {
    LOG(ERROR) << "warning: path '" << path
               << "' claims to be content-addressed but isn't";
  };

  if (absl::StartsWith(ca, "text:")) {
    auto hash_ = Hash::deserialize(std::string_view(ca).substr(5));
    Hash hash = Hash::unwrap_throw(hash_);
    if (store.makeTextPath(storePathToName(path), hash, references) == path) {
      return true;
    }
    warn();

  }

  else if (absl::StartsWith(ca, "fixed:")) {
    bool recursive = ca.compare(6, 2, "r:") == 0;
    auto hash_ =
        Hash::deserialize(std::string_view(ca).substr(recursive ? 8 : 6));
    Hash hash = Hash::unwrap_throw(hash_);
    if (references.empty() &&
        store.makeFixedOutputPath(recursive, hash, storePathToName(path)) ==
            path) {
      return true;
    }
    warn();
  }

  return false;
}

size_t ValidPathInfo::checkSignatures(const Store& store,
                                      const PublicKeys& publicKeys) const {
  if (isContentAddressed(store)) {
    return maxSigs;
  }

  size_t good = 0;
  for (auto& sig : sigs) {
    if (checkSignature(publicKeys, sig)) {
      good++;
    }
  }
  return good;
}

bool ValidPathInfo::checkSignature(const PublicKeys& publicKeys,
                                   const std::string& sig) const {
  return verifyDetached(fingerprint(), sig, publicKeys);
}

Strings ValidPathInfo::shortRefs() const {
  Strings refs;
  for (auto& r : references) {
    refs.push_back(baseNameOf(r));
  }
  return refs;
}

std::string makeFixedOutputCA(bool recursive, const Hash& hash) {
  return "fixed:" + (recursive ? std::string("r:") : "") + hash.to_string();
}

void Store::addToStore(const ValidPathInfo& info, Source& narSource,
                       RepairFlag repair, CheckSigsFlag checkSigs,
                       std::shared_ptr<FSAccessor> accessor) {
  addToStore(info, make_ref<std::string>(narSource.drain()), repair, checkSigs,
             std::move(accessor));
}

void Store::addToStore(const ValidPathInfo& info, const ref<std::string>& nar,
                       RepairFlag repair, CheckSigsFlag checkSigs,
                       std::shared_ptr<FSAccessor> accessor) {
  StringSource source(*nar);
  addToStore(info, source, repair, checkSigs, std::move(accessor));
}

}  // namespace nix

#include "libstore/local-store.hh"
#include "libstore/remote-store.hh"

namespace nix {

RegisterStoreImplementation::Implementations*
    RegisterStoreImplementation::implementations = nullptr;

/* Split URI into protocol+hierarchy part and its parameter set. */
std::pair<std::string, Store::Params> splitUriAndParams(
    const std::string& uri_) {
  auto uri(uri_);
  Store::Params params;
  auto q = uri.find('?');
  if (q != std::string::npos) {
    Strings parts =
        absl::StrSplit(uri.substr(q + 1), absl::ByChar('&'), absl::SkipEmpty());
    for (const auto& s : parts) {
      auto e = s.find('=');
      if (e != std::string::npos) {
        auto value = s.substr(e + 1);
        std::string decoded;
        for (size_t i = 0; i < value.size();) {
          if (value[i] == '%') {
            if (i + 2 >= value.size()) {
              throw Error("invalid URI parameter '%s'", value);
            }
            try {
              decoded += std::stoul(std::string(value, i + 1, 2), nullptr, 16);
              i += 3;
            } catch (...) {
              throw Error("invalid URI parameter '%s'", value);
            }
          } else {
            decoded += value[i++];
          }
        }
        params[s.substr(0, e)] = decoded;
      }
    }
    uri = uri_.substr(0, q);
  }
  return {uri, params};
}

ref<Store> openStore(const std::string& uri_,
                     const Store::Params& extraParams) {
  auto [uri, uriParams] = splitUriAndParams(uri_);
  auto params = extraParams;
  params.insert(uriParams.begin(), uriParams.end());

  for (const auto& fun : *RegisterStoreImplementation::implementations) {
    auto store = fun(uri, params);
    if (store) {
      store->warnUnknownSettings();
      return ref<Store>(store);
    }
  }

  throw Error("don't know how to open Nix store '%s'", uri);
}

StoreType getStoreType(const std::string& uri, const std::string& stateDir) {
  if (uri == "daemon") {
    return tDaemon;
  }
  if (uri == "local" || absl::StartsWith(uri, "/")) {
    return tLocal;
  } else if (uri.empty() || uri == "auto") {
    if (access(stateDir.c_str(), R_OK | W_OK) == 0) {
      return tLocal;
    }
    if (pathExists(settings.nixDaemonSocketFile)) {
      return tDaemon;
    } else {
      return tLocal;
    }
  } else {
    return tOther;
  }
}

static RegisterStoreImplementation regStore([](const std::string& uri,
                                               const Store::Params& params)
                                                -> std::shared_ptr<Store> {
  switch (getStoreType(uri, get(params, "state", settings.nixStateDir))) {
    case tDaemon: {
      auto daemon_socket_uri =
          absl::StrCat("unix://", settings.nixDaemonSocketFile);
      auto channel = grpc::CreateChannel(daemon_socket_uri,
                                         grpc::InsecureChannelCredentials());
      return std::shared_ptr<Store>(std::make_shared<nix::store::RpcStore>(
          daemon_socket_uri, params, proto::WorkerService::NewStub(channel)));
    }
    case tLocal: {
      Store::Params params2 = params;
      if (absl::StartsWith(uri, "/")) {
        params2["root"] = uri;
      }
      return std::shared_ptr<Store>(std::make_shared<LocalStore>(params2));
    }
    default:
      return nullptr;
  }
});

std::list<ref<Store>> getDefaultSubstituters() {
  static auto stores([]() {
    std::list<ref<Store>> stores;

    StringSet done;

    auto addStore = [&](const std::string& uri) {
      if (done.count(uri) != 0u) {
        return;
      }
      done.insert(uri);
      try {
        stores.push_back(openStore(uri));
      } catch (Error& e) {
        LOG(WARNING) << e.what();
      }
    };

    for (const auto& uri : settings.substituters.get()) {
      addStore(uri);
    }

    for (const auto& uri : settings.extraSubstituters.get()) {
      addStore(uri);
    }

    stores.sort([](ref<Store>& a, ref<Store>& b) {
      return a->getPriority() < b->getPriority();
    });

    return stores;
  }());

  return stores;
}

}  // namespace nix
