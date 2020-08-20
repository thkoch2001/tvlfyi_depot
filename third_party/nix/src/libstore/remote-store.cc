#include "libstore/remote-store.hh"

#include <cerrno>
#include <cstring>

#include <absl/status/status.h>
#include <absl/strings/ascii.h>
#include <fcntl.h>
#include <glog/logging.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

#include "libstore/derivations.hh"
#include "libstore/globals.hh"
#include "libstore/worker-protocol.hh"
#include "libutil/affinity.hh"
#include "libutil/archive.hh"
#include "libutil/finally.hh"
#include "libutil/pool.hh"
#include "libutil/serialise.hh"
#include "libutil/util.hh"

namespace nix {

Path readStorePath(Store& store, Source& from) {
  Path path = readString(from);
  store.assertStorePath(path);
  return path;
}

template <class T>
T readStorePaths(Store& store, Source& from) {
  T paths = readStrings<T>(from);
  for (auto& i : paths) {
    store.assertStorePath(i);
  }
  return paths;
}

template PathSet readStorePaths(Store& store, Source& from);
template Paths readStorePaths(Store& store, Source& from);

/* TODO: Separate these store impls into different files, give them better names
 */
RemoteStore::RemoteStore(const Params& params)
    : Store(params),
      connections(make_ref<Pool<Connection>>(
          std::max(1, (int)maxConnections),
          [this]() { return openConnectionWrapper(); },
          [this](const ref<Connection>& r) {
            return r->to.good() && r->from.good() &&
                   std::chrono::duration_cast<std::chrono::seconds>(
                       std::chrono::steady_clock::now() - r->startTime)
                           .count() < maxConnectionAge;
          })) {}

ref<RemoteStore::Connection> RemoteStore::openConnectionWrapper() {
  if (failed) {
    throw Error("opening a connection to remote store '%s' previously failed",
                getUri());
  }
  try {
    return openConnection();
  } catch (...) {
    failed = true;
    throw;
  }
}

void RemoteStore::initConnection(Connection& conn) {
  /* Send the magic greeting, check for the reply. */
  try {
    conn.to << WORKER_MAGIC_1;
    conn.to.flush();
    unsigned int magic = readInt(conn.from);
    if (magic != WORKER_MAGIC_2) {
      throw Error("protocol mismatch");
    }

    conn.from >> conn.daemonVersion;
    if (GET_PROTOCOL_MAJOR(conn.daemonVersion) !=
        GET_PROTOCOL_MAJOR(PROTOCOL_VERSION)) {
      throw Error("Nix daemon protocol version not supported");
    }
    if (GET_PROTOCOL_MINOR(conn.daemonVersion) < 10) {
      throw Error("the Nix daemon version is too old");
    }
    conn.to << PROTOCOL_VERSION;

    if (GET_PROTOCOL_MINOR(conn.daemonVersion) >= 14) {
      int cpu = sameMachine() && settings.lockCPU ? lockToCurrentCPU() : -1;
      if (cpu != -1) {
        conn.to << 1 << cpu;
      } else {
        conn.to << 0;
      }
    }

    if (GET_PROTOCOL_MINOR(conn.daemonVersion) >= 11) {
      conn.to << 0u;
    }

    auto ex = conn.processStderr();
    if (ex) {
      std::rethrow_exception(ex);
    }
  } catch (Error& e) {
    throw Error("cannot open connection to remote store '%s': %s", getUri(),
                e.what());
  }

  setOptions(conn);
}

void RemoteStore::setOptions(Connection& conn) {
  conn.to << wopSetOptions << static_cast<uint64_t>(settings.keepFailed)
          << static_cast<uint64_t>(settings.keepGoing)
          << static_cast<uint64_t>(settings.tryFallback)
          << /* previously: verbosity = */ 0 << settings.maxBuildJobs
          << settings.maxSilentTime << 1u
          << /* previously: remote verbosity = */ 0 << 0  // obsolete log type
          << 0 /* obsolete print build trace */
          << settings.buildCores
          << static_cast<uint64_t>(settings.useSubstitutes);

  if (GET_PROTOCOL_MINOR(conn.daemonVersion) >= 12) {
    std::map<std::string, Config::SettingInfo> overrides;
    globalConfig.getSettings(overrides, true);
    overrides.erase(settings.keepFailed.name);
    overrides.erase(settings.keepGoing.name);
    overrides.erase(settings.tryFallback.name);
    overrides.erase(settings.maxBuildJobs.name);
    overrides.erase(settings.maxSilentTime.name);
    overrides.erase(settings.buildCores.name);
    overrides.erase(settings.useSubstitutes.name);
    overrides.erase(settings.showTrace.name);
    conn.to << overrides.size();
    for (auto& i : overrides) {
      conn.to << i.first << i.second.value;
    }
  }

  auto ex = conn.processStderr();
  if (ex) {
    std::rethrow_exception(ex);
  }
}

/* A wrapper around Pool<RemoteStore::Connection>::Handle that marks
   the connection as bad (causing it to be closed) if a non-daemon
   exception is thrown before the handle is closed. Such an exception
   causes a deviation from the expected protocol and therefore a
   desynchronization between the client and daemon. */
struct ConnectionHandle {
  Pool<RemoteStore::Connection>::Handle handle;
  bool daemonException = false;

  explicit ConnectionHandle(Pool<RemoteStore::Connection>::Handle&& handle)
      : handle(std::move(handle)) {}

  ConnectionHandle(ConnectionHandle&& h) : handle(std::move(h.handle)) {}

  ~ConnectionHandle() {
    if (!daemonException && (std::uncaught_exceptions() != 0)) {
      handle.markBad();
      // TODO(tazjin): are these types of things supposed to be DEBUG?
      DLOG(INFO) << "closing daemon connection because of an exception";
    }
  }

  RemoteStore::Connection* operator->() { return &*handle; }

  void processStderr(Sink* sink = nullptr, Source* source = nullptr) {
    auto ex = handle->processStderr(sink, source);
    if (ex) {
      daemonException = true;
      std::rethrow_exception(ex);
    }
  }
};

ConnectionHandle RemoteStore::getConnection() {
  return ConnectionHandle(connections->get());
}

bool RemoteStore::isValidPathUncached(const Path& path) {
  auto conn(getConnection());
  conn->to << wopIsValidPath << path;
  conn.processStderr();
  return readInt(conn->from) != 0u;
}

PathSet RemoteStore::queryValidPaths(const PathSet& paths,
                                     SubstituteFlag maybeSubstitute) {
  auto conn(getConnection());
  if (GET_PROTOCOL_MINOR(conn->daemonVersion) < 12) {
    PathSet res;
    for (auto& i : paths) {
      if (isValidPath(i)) {
        res.insert(i);
      }
    }
    return res;
  }
  conn->to << wopQueryValidPaths << paths;
  conn.processStderr();
  return readStorePaths<PathSet>(*this, conn->from);
}

PathSet RemoteStore::queryAllValidPaths() {
  auto conn(getConnection());
  conn->to << wopQueryAllValidPaths;
  conn.processStderr();
  return readStorePaths<PathSet>(*this, conn->from);
}

PathSet RemoteStore::querySubstitutablePaths(const PathSet& paths) {
  auto conn(getConnection());
  if (GET_PROTOCOL_MINOR(conn->daemonVersion) < 12) {
    PathSet res;
    for (auto& i : paths) {
      conn->to << wopHasSubstitutes << i;
      conn.processStderr();
      if (readInt(conn->from) != 0u) {
        res.insert(i);
      }
    }
    return res;
  }
  conn->to << wopQuerySubstitutablePaths << paths;
  conn.processStderr();
  return readStorePaths<PathSet>(*this, conn->from);
}

void RemoteStore::querySubstitutablePathInfos(const PathSet& paths,
                                              SubstitutablePathInfos& infos) {
  if (paths.empty()) {
    return;
  }

  auto conn(getConnection());

  if (GET_PROTOCOL_MINOR(conn->daemonVersion) < 12) {
    for (auto& i : paths) {
      SubstitutablePathInfo info;
      conn->to << wopQuerySubstitutablePathInfo << i;
      conn.processStderr();
      unsigned int reply = readInt(conn->from);
      if (reply == 0) {
        continue;
      }
      info.deriver = readString(conn->from);
      if (!info.deriver.empty()) {
        assertStorePath(info.deriver);
      }
      info.references = readStorePaths<PathSet>(*this, conn->from);
      info.downloadSize = readLongLong(conn->from);
      info.narSize = readLongLong(conn->from);
      infos[i] = info;
    }

  } else {
    conn->to << wopQuerySubstitutablePathInfos << paths;
    conn.processStderr();
    auto count = readNum<size_t>(conn->from);
    for (size_t n = 0; n < count; n++) {
      Path path = readStorePath(*this, conn->from);
      SubstitutablePathInfo& info(infos[path]);
      info.deriver = readString(conn->from);
      if (!info.deriver.empty()) {
        assertStorePath(info.deriver);
      }
      info.references = readStorePaths<PathSet>(*this, conn->from);
      info.downloadSize = readLongLong(conn->from);
      info.narSize = readLongLong(conn->from);
    }
  }
}

void RemoteStore::queryPathInfoUncached(
    const Path& path,
    Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept {
  try {
    std::shared_ptr<ValidPathInfo> info;
    {
      auto conn(getConnection());
      conn->to << wopQueryPathInfo << path;
      try {
        conn.processStderr();
      } catch (Error& e) {
        // Ugly backwards compatibility hack.
        if (e.msg().find("is not valid") != std::string::npos) {
          throw InvalidPath(e.what());
        }
        throw;
      }
      if (GET_PROTOCOL_MINOR(conn->daemonVersion) >= 17) {
        bool valid;
        conn->from >> valid;
        if (!valid) {
          throw InvalidPath(format("path '%s' is not valid") % path);
        }
      }
      info = std::make_shared<ValidPathInfo>();
      info->path = path;
      info->deriver = readString(conn->from);
      if (!info->deriver.empty()) {
        assertStorePath(info->deriver);
      }
      auto hash_ = Hash::deserialize(readString(conn->from), htSHA256);
      info->narHash = Hash::unwrap_throw(hash_);
      info->references = readStorePaths<PathSet>(*this, conn->from);
      conn->from >> info->registrationTime >> info->narSize;
      if (GET_PROTOCOL_MINOR(conn->daemonVersion) >= 16) {
        conn->from >> info->ultimate;
        info->sigs = readStrings<StringSet>(conn->from);
        conn->from >> info->ca;
      }
    }
    callback(std::move(info));
  } catch (...) {
    callback.rethrow();
  }
}

void RemoteStore::queryReferrers(const Path& path, PathSet& referrers) {
  auto conn(getConnection());
  conn->to << wopQueryReferrers << path;
  conn.processStderr();
  auto referrers2 = readStorePaths<PathSet>(*this, conn->from);
  referrers.insert(referrers2.begin(), referrers2.end());
}

PathSet RemoteStore::queryValidDerivers(const Path& path) {
  auto conn(getConnection());
  conn->to << wopQueryValidDerivers << path;
  conn.processStderr();
  return readStorePaths<PathSet>(*this, conn->from);
}

PathSet RemoteStore::queryDerivationOutputs(const Path& path) {
  auto conn(getConnection());
  conn->to << wopQueryDerivationOutputs << path;
  conn.processStderr();
  return readStorePaths<PathSet>(*this, conn->from);
}

PathSet RemoteStore::queryDerivationOutputNames(const Path& path) {
  auto conn(getConnection());
  conn->to << wopQueryDerivationOutputNames << path;
  conn.processStderr();
  return readStrings<PathSet>(conn->from);
}

Path RemoteStore::queryPathFromHashPart(const std::string& hashPart) {
  auto conn(getConnection());
  conn->to << wopQueryPathFromHashPart << hashPart;
  conn.processStderr();
  Path path = readString(conn->from);
  if (!path.empty()) {
    assertStorePath(path);
  }
  return path;
}

void RemoteStore::addToStore(const ValidPathInfo& info, Source& source,
                             RepairFlag repair, CheckSigsFlag checkSigs,
                             std::shared_ptr<FSAccessor> accessor) {
  auto conn(getConnection());

  if (GET_PROTOCOL_MINOR(conn->daemonVersion) < 18) {
    conn->to << wopImportPaths;

    auto source2 = sinkToSource([&](Sink& sink) {
      sink << 1  // == path follows
          ;
      copyNAR(source, sink);
      sink << exportMagic << info.path << info.references << info.deriver
           << 0  // == no legacy signature
           << 0  // == no path follows
          ;
    });

    conn.processStderr(nullptr, source2.get());

    auto importedPaths = readStorePaths<PathSet>(*this, conn->from);
    assert(importedPaths.size() <= 1);
  }

  else {
    conn->to << wopAddToStoreNar << info.path << info.deriver
             << info.narHash.to_string(Base16, false) << info.references
             << info.registrationTime << info.narSize << info.ultimate
             << info.sigs << info.ca << repair << !checkSigs;
    bool tunnel = GET_PROTOCOL_MINOR(conn->daemonVersion) >= 21;
    if (!tunnel) {
      copyNAR(source, conn->to);
    }
    conn.processStderr(nullptr, tunnel ? &source : nullptr);
  }
}

Path RemoteStore::addToStore(const std::string& name, const Path& _srcPath,
                             bool recursive, HashType hashAlgo,
                             PathFilter& filter, RepairFlag repair) {
  if (repair != 0u) {
    throw Error(
        "repairing is not supported when building through the Nix daemon");
  }

  auto conn(getConnection());

  Path srcPath(absPath(_srcPath));

  conn->to << wopAddToStore << name
           << ((hashAlgo == htSHA256 && recursive)
                   ? 0
                   : 1) /* backwards compatibility hack */
           << (recursive ? 1 : 0) << printHashType(hashAlgo);

  try {
    conn->to.written = 0;
    conn->to.warn = true;
    connections->incCapacity();
    {
      Finally cleanup([&]() { connections->decCapacity(); });
      dumpPath(srcPath, conn->to, filter);
    }
    conn->to.warn = false;
    conn.processStderr();
  } catch (SysError& e) {
    /* Daemon closed while we were sending the path. Probably OOM
       or I/O error. */
    if (e.errNo == EPIPE) {
      try {
        conn.processStderr();
      } catch (EndOfFile& e) {
      }
    }
    throw;
  }

  return readStorePath(*this, conn->from);
}

Path RemoteStore::addTextToStore(const std::string& name, const std::string& s,
                                 const PathSet& references, RepairFlag repair) {
  if (repair != 0u) {
    throw Error(
        "repairing is not supported when building through the Nix daemon");
  }

  auto conn(getConnection());
  conn->to << wopAddTextToStore << name << s << references;

  conn.processStderr();
  return readStorePath(*this, conn->from);
}

absl::Status RemoteStore::buildPaths(std::ostream& /* log_sink */,
                                     const PathSet& drvPaths,
                                     BuildMode build_mode) {
  auto conn(getConnection());
  conn->to << wopBuildPaths;
  if (GET_PROTOCOL_MINOR(conn->daemonVersion) >= 13) {
    conn->to << drvPaths;
    if (GET_PROTOCOL_MINOR(conn->daemonVersion) >= 15) {
      conn->to << build_mode;
    } else if (build_mode != bmNormal) {
      /* Old daemons did not take a 'buildMode' parameter, so we
         need to validate it here on the client side.  */
      return absl::Status(
          absl::StatusCode::kInvalidArgument,
          "repairing or checking is not supported when building through the "
          "Nix daemon");
    }
  } else {
    /* For backwards compatibility with old daemons, strip output
       identifiers. */
    PathSet drvPaths2;
    for (auto& i : drvPaths) {
      drvPaths2.insert(std::string(i, 0, i.find('!')));
    }
    conn->to << drvPaths2;
  }
  conn.processStderr();
  readInt(conn->from);

  return absl::OkStatus();
}

BuildResult RemoteStore::buildDerivation(const Path& drvPath,
                                         const BasicDerivation& drv,
                                         BuildMode buildMode) {
  auto conn(getConnection());
  conn->to << wopBuildDerivation << drvPath << drv << buildMode;
  conn.processStderr();
  BuildResult res;
  unsigned int status;
  conn->from >> status >> res.errorMsg;
  res.status = static_cast<BuildResult::Status>(status);
  return res;
}

void RemoteStore::ensurePath(const Path& path) {
  auto conn(getConnection());
  conn->to << wopEnsurePath << path;
  conn.processStderr();
  readInt(conn->from);
}

void RemoteStore::addTempRoot(const Path& path) {
  auto conn(getConnection());
  conn->to << wopAddTempRoot << path;
  conn.processStderr();
  readInt(conn->from);
}

void RemoteStore::addIndirectRoot(const Path& path) {
  auto conn(getConnection());
  conn->to << wopAddIndirectRoot << path;
  conn.processStderr();
  readInt(conn->from);
}

void RemoteStore::syncWithGC() {
  auto conn(getConnection());
  conn->to << wopSyncWithGC;
  conn.processStderr();
  readInt(conn->from);
}

Roots RemoteStore::findRoots(bool censor) {
  auto conn(getConnection());
  conn->to << wopFindRoots;
  conn.processStderr();
  auto count = readNum<size_t>(conn->from);
  Roots result;
  while ((count--) != 0u) {
    Path link = readString(conn->from);
    Path target = readStorePath(*this, conn->from);
    result[target].emplace(link);
  }
  return result;
}

void RemoteStore::collectGarbage(const GCOptions& options, GCResults& results) {
  auto conn(getConnection());

  conn->to << wopCollectGarbage << options.action << options.pathsToDelete
           << static_cast<uint64_t>(options.ignoreLiveness)
           << options.maxFreed
           /* removed options */
           << 0 << 0 << 0;

  conn.processStderr();

  results.paths = readStrings<PathSet>(conn->from);
  results.bytesFreed = readLongLong(conn->from);
  readLongLong(conn->from);  // obsolete

  {
    auto state_(Store::state.lock());
    state_->pathInfoCache.clear();
  }
}

void RemoteStore::optimiseStore() {
  auto conn(getConnection());
  conn->to << wopOptimiseStore;
  conn.processStderr();
  readInt(conn->from);
}

bool RemoteStore::verifyStore(bool checkContents, RepairFlag repair) {
  auto conn(getConnection());
  conn->to << wopVerifyStore << static_cast<uint64_t>(checkContents) << repair;
  conn.processStderr();
  return readInt(conn->from) != 0u;
}

void RemoteStore::addSignatures(const Path& storePath, const StringSet& sigs) {
  auto conn(getConnection());
  conn->to << wopAddSignatures << storePath << sigs;
  conn.processStderr();
  readInt(conn->from);
}

void RemoteStore::queryMissing(const PathSet& targets, PathSet& willBuild,
                               PathSet& willSubstitute, PathSet& unknown,
                               unsigned long long& downloadSize,
                               unsigned long long& narSize) {
  {
    auto conn(getConnection());
    if (GET_PROTOCOL_MINOR(conn->daemonVersion) < 19) {
      // Don't hold the connection handle in the fallback case
      // to prevent a deadlock.
      goto fallback;
    }
    conn->to << wopQueryMissing << targets;
    conn.processStderr();
    willBuild = readStorePaths<PathSet>(*this, conn->from);
    willSubstitute = readStorePaths<PathSet>(*this, conn->from);
    unknown = readStorePaths<PathSet>(*this, conn->from);
    conn->from >> downloadSize >> narSize;
    return;
  }

fallback:
  return Store::queryMissing(targets, willBuild, willSubstitute, unknown,
                             downloadSize, narSize);
}

void RemoteStore::connect() { auto conn(getConnection()); }

unsigned int RemoteStore::getProtocol() {
  auto conn(connections->get());
  return conn->daemonVersion;
}

void RemoteStore::flushBadConnections() { connections->flushBad(); }

RemoteStore::Connection::~Connection() {
  try {
    to.flush();
  } catch (...) {
    ignoreException();
  }
}

std::exception_ptr RemoteStore::Connection::processStderr(Sink* sink,
                                                          Source* source) {
  to.flush();

  while (true) {
    auto msg = readNum<uint64_t>(from);

    if (msg == STDERR_WRITE) {
      std::string s = readString(from);
      if (sink == nullptr) {
        throw Error("no sink");
      }
      (*sink)(s);
    }

    else if (msg == STDERR_READ) {
      if (source == nullptr) {
        throw Error("no source");
      }
      auto len = readNum<size_t>(from);
      auto buf = std::make_unique<unsigned char[]>(len);
      writeString(buf.get(), source->read(buf.get(), len), to);
      to.flush();
    }

    else if (msg == STDERR_ERROR) {
      std::string error = readString(from);
      unsigned int status = readInt(from);
      return std::make_exception_ptr(Error(status, error));
    }

    else if (msg == STDERR_NEXT) {
      LOG(ERROR) << absl::StripTrailingAsciiWhitespace(readString(from));
    }

    else if (msg == STDERR_START_ACTIVITY) {
      LOG(INFO) << readString(from);
    }

    else if (msg == STDERR_LAST) {
      break;
    }

    else {
      throw Error("got unknown message type %x from Nix daemon", msg);
    }
  }

  return nullptr;
}

}  // namespace nix
