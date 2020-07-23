#include <algorithm>
#include <cerrno>
#include <climits>
#include <csignal>
#include <cstring>

#include <fcntl.h>
#include <glog/logging.h>
#include <grp.h>
#include <pwd.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

#include "libmain/shared.hh"
#include "libproto/worker.pb.h"
#include "libstore/derivations.hh"
#include "libstore/globals.hh"
#include "libstore/local-store.hh"
#include "libstore/worker-protocol.hh"
#include "libutil/affinity.hh"
#include "libutil/archive.hh"
#include "libutil/finally.hh"
#include "libutil/monitor-fd.hh"
#include "libutil/serialise.hh"
#include "libutil/util.hh"
#include "nix/legacy.hh"

using namespace nix;

#ifndef __linux__
#define SPLICE_F_MOVE 0
static ssize_t splice(int fd_in, void* off_in, int fd_out, void* off_out,
                      size_t len, unsigned int flags) {
  /* We ignore most parameters, we just have them for conformance with the linux
   * syscall */
  std::vector<char> buf(8192);
  auto read_count = read(fd_in, buf.data(), buf.size());
  if (read_count == -1) {
    return read_count;
  }
  auto write_count = decltype(read_count)(0);
  while (write_count < read_count) {
    auto res =
        write(fd_out, buf.data() + write_count, read_count - write_count);
    if (res == -1) {
      return res;
    }
    write_count += res;
  }
  return read_count;
}
#endif

static FdSource from(STDIN_FILENO);
static FdSink to(STDOUT_FILENO);

/* Logger that forwards log messages to the client, *if* we're in a
   state where the protocol allows it (i.e., when canSendStderr is
   true). */
struct TunnelLogger {
  struct State {
    bool canSendStderr = false;
    std::vector<std::string> pendingMsgs;
  };

  Sync<State> state_;

  unsigned int clientVersion;

  explicit TunnelLogger(unsigned int clientVersion)
      : clientVersion(clientVersion) {}

  void enqueueMsg(const std::string& s) {
    auto state(state_.lock());

    if (state->canSendStderr) {
      assert(state->pendingMsgs.empty());
      try {
        to(s);
        to.flush();
      } catch (...) {
        /* Write failed; that means that the other side is
           gone. */
        state->canSendStderr = false;
        throw;
      }
    } else {
      state->pendingMsgs.push_back(s);
    }
  }

  void log(const FormatOrString& fs) {
    StringSink buf;
    buf << STDERR_NEXT << (fs.s + "\n");
    enqueueMsg(*buf.s);
  }

  /* startWork() means that we're starting an operation for which we
    want to send out stderr to the client. */
  void startWork() {
    auto state(state_.lock());
    state->canSendStderr = true;

    for (auto& msg : state->pendingMsgs) {
      to(msg);
    }

    state->pendingMsgs.clear();

    to.flush();
  }

  /* stopWork() means that we're done; stop sending stderr to the
     client. */
  void stopWork(bool success = true, const std::string& msg = "",
                unsigned int status = 0) {
    auto state(state_.lock());

    state->canSendStderr = false;

    if (success) {
      to << STDERR_LAST;
    } else {
      to << STDERR_ERROR << msg;
      if (status != 0) {
        to << status;
      }
    }
  }

  void startActivity(const std::string& s) {
    DLOG(INFO) << "startActivity(" << s << ")";
    if (GET_PROTOCOL_MINOR(clientVersion) < 20) {
      if (!s.empty()) {
        LOG(INFO) << s;
      }
      return;
    }

    StringSink buf;
    buf << STDERR_START_ACTIVITY << s;
    enqueueMsg(*buf.s);
  }
};

struct TunnelSink : Sink {
  Sink& to;
  explicit TunnelSink(Sink& to) : to(to) {}
  void operator()(const unsigned char* data, size_t len) override {
    to << STDERR_WRITE;
    writeString(data, len, to);
  }
};

struct TunnelSource : BufferedSource {
  Source& from;
  explicit TunnelSource(Source& from) : from(from) {}

 protected:
  size_t readUnbuffered(unsigned char* data, size_t len) override {
    to << STDERR_READ << len;
    to.flush();
    size_t n = readString(data, len, from);
    if (n == 0) {
      throw EndOfFile("unexpected end-of-file");
    }
    return n;
  }
};

/* If the NAR archive contains a single file at top-level, then save
   the contents of the file to `s'.  Otherwise barf. */
struct RetrieveRegularNARSink : ParseSink {
  bool regular{true};
  std::string s;

  RetrieveRegularNARSink() {}

  void createDirectory(const Path& path) override { regular = false; }

  void receiveContents(unsigned char* data, unsigned int len) override {
    s.append((const char*)data, len);
  }

  void createSymlink(const Path& path, const std::string& target) override {
    regular = false;
  }
};

static void performOp(TunnelLogger* logger, const std::shared_ptr<Store>& store,
                      bool trusted, unsigned int clientVersion, Source& from,
                      Sink& to, unsigned int op) {
  switch (op) {
    case wopIsValidPath: {
      /* 'readStorePath' could raise an error leading to the connection
         being closed.  To be able to recover from an invalid path error,
         call 'startWork' early, and do 'assertStorePath' afterwards so
         that the 'Error' exception handler doesn't close the
         connection.  */
      Path path = readString(from);
      logger->startWork();
      store->assertStorePath(path);
      bool result = store->isValidPath(path);
      logger->stopWork();
      to << static_cast<uint64_t>(result);
      break;
    }

    case wopQueryValidPaths: {
      auto paths = readStorePaths<PathSet>(*store, from);
      logger->startWork();
      PathSet res = store->queryValidPaths(paths);
      logger->stopWork();
      to << res;
      break;
    }

    case wopHasSubstitutes: {
      Path path = readStorePath(*store, from);
      logger->startWork();
      PathSet res = store->querySubstitutablePaths({path});
      logger->stopWork();
      to << static_cast<uint64_t>(res.find(path) != res.end());
      break;
    }

    case wopQuerySubstitutablePaths: {
      auto paths = readStorePaths<PathSet>(*store, from);
      logger->startWork();
      PathSet res = store->querySubstitutablePaths(paths);
      logger->stopWork();
      to << res;
      break;
    }

    case wopQueryPathHash: {
      Path path = readStorePath(*store, from);
      logger->startWork();
      auto hash = store->queryPathInfo(path)->narHash;
      logger->stopWork();
      to << hash.to_string(Base16, false);
      break;
    }

    case wopQueryReferences:
    case wopQueryReferrers:
    case wopQueryValidDerivers:
    case wopQueryDerivationOutputs: {
      Path path = readStorePath(*store, from);
      logger->startWork();
      PathSet paths;
      if (op == wopQueryReferences) {
        paths = store->queryPathInfo(path)->references;
      } else if (op == wopQueryReferrers) {
        store->queryReferrers(path, paths);
      } else if (op == wopQueryValidDerivers) {
        paths = store->queryValidDerivers(path);
      } else {
        paths = store->queryDerivationOutputs(path);
      }
      logger->stopWork();
      to << paths;
      break;
    }

    case wopQueryDerivationOutputNames: {
      Path path = readStorePath(*store, from);
      logger->startWork();
      StringSet names;
      names = store->queryDerivationOutputNames(path);
      logger->stopWork();
      to << names;
      break;
    }

    case wopQueryDeriver: {
      Path path = readStorePath(*store, from);
      logger->startWork();
      auto deriver = store->queryPathInfo(path)->deriver;
      logger->stopWork();
      to << deriver;
      break;
    }

    case wopQueryPathFromHashPart: {
      std::string hashPart = readString(from);
      logger->startWork();
      Path path = store->queryPathFromHashPart(hashPart);
      logger->stopWork();
      to << path;
      break;
    }

    case wopAddToStore: {
      bool fixed;
      bool recursive;
      std::string s;
      std::string baseName;
      from >> baseName >> fixed /* obsolete */ >> recursive >> s;
      /* Compatibility hack. */
      if (!fixed) {
        s = "sha256";
        recursive = true;
      }
      HashType hashAlgo = parseHashType(s);

      TeeSource savedNAR(from);
      RetrieveRegularNARSink savedRegular;

      if (recursive) {
        /* Get the entire NAR dump from the client and save it to
           a string so that we can pass it to
           addToStoreFromDump(). */
        ParseSink sink; /* null sink; just parse the NAR */
        parseDump(sink, savedNAR);
      } else {
        parseDump(savedRegular, from);
      }

      logger->startWork();
      if (!savedRegular.regular) {
        throw Error("regular file expected");
      }

      auto store2 = std::dynamic_pointer_cast<LocalStore>(store);
      if (!store2) {
        throw Error("operation is only supported by LocalStore");
      }

      Path path = store2->addToStoreFromDump(
          recursive ? *savedNAR.data : savedRegular.s, baseName, recursive,
          hashAlgo);
      logger->stopWork();

      to << path;
      break;
    }

    case wopAddTextToStore: {
      std::string suffix = readString(from);
      std::string s = readString(from);
      auto refs = readStorePaths<PathSet>(*store, from);
      logger->startWork();
      Path path = store->addTextToStore(suffix, s, refs, NoRepair);
      logger->stopWork();
      to << path;
      break;
    }

    case wopExportPath: {
      Path path = readStorePath(*store, from);
      readInt(from);  // obsolete
      logger->startWork();
      TunnelSink sink(to);
      store->exportPath(path, sink);
      logger->stopWork();
      to << 1;
      break;
    }

    case wopImportPaths: {
      logger->startWork();
      TunnelSource source(from);
      Paths paths = store->importPaths(source, nullptr,
                                       trusted ? NoCheckSigs : CheckSigs);
      logger->stopWork();
      to << paths;
      break;
    }

    case wopBuildPaths: {
      auto drvs = readStorePaths<PathSet>(*store, from);
      BuildMode mode = bmNormal;
      if (GET_PROTOCOL_MINOR(clientVersion) >= 15) {
        mode = (BuildMode)readInt(from);

        /* Repairing is not atomic, so disallowed for "untrusted"
           clients.  */
        if (mode == bmRepair && !trusted) {
          throw Error(
              "repairing is not allowed because you are not in "
              "'trusted-users'");
        }
      }
      logger->startWork();
      store->buildPaths(drvs, mode);
      logger->stopWork();
      to << 1;
      break;
    }

    case wopBuildDerivation: {
      Path drvPath = readStorePath(*store, from);
      BasicDerivation drv;
      readDerivation(from, *store, drv);
      auto buildMode = (BuildMode)readInt(from);
      logger->startWork();
      if (!trusted) {
        throw Error("you are not privileged to build derivations");
      }
      auto res = store->buildDerivation(drvPath, drv, buildMode);
      logger->stopWork();
      to << res.status << res.errorMsg;
      break;
    }

    case wopEnsurePath: {
      Path path = readStorePath(*store, from);
      logger->startWork();
      store->ensurePath(path);
      logger->stopWork();
      to << 1;
      break;
    }

    case wopAddTempRoot: {
      Path path = readStorePath(*store, from);
      logger->startWork();
      store->addTempRoot(path);
      logger->stopWork();
      to << 1;
      break;
    }

    case wopAddIndirectRoot: {
      Path path = absPath(readString(from));
      logger->startWork();
      store->addIndirectRoot(path);
      logger->stopWork();
      to << 1;
      break;
    }

    case wopSyncWithGC: {
      logger->startWork();
      store->syncWithGC();
      logger->stopWork();
      to << 1;
      break;
    }

    case wopFindRoots: {
      logger->startWork();
      Roots roots = store->findRoots(!trusted);
      logger->stopWork();

      size_t size = 0;
      for (auto& i : roots) {
        size += i.second.size();
      }

      to << size;

      for (auto& [target, links] : roots) {
        for (auto& link : links) {
          to << link << target;
        }
      }

      break;
    }

    case wopCollectGarbage: {
      GCOptions options;
      options.action = (GCOptions::GCAction)readInt(from);
      options.pathsToDelete = readStorePaths<PathSet>(*store, from);
      from >> options.ignoreLiveness >> options.maxFreed;
      // obsolete fields
      readInt(from);
      readInt(from);
      readInt(from);

      GCResults results;

      logger->startWork();
      if (options.ignoreLiveness) {
        throw Error("you are not allowed to ignore liveness");
      }
      store->collectGarbage(options, results);
      logger->stopWork();

      to << results.paths << results.bytesFreed << 0 /* obsolete */;

      break;
    }

    case wopSetOptions: {
      settings.keepFailed = readInt(from) != 0u;
      settings.keepGoing = readInt(from) != 0u;
      settings.tryFallback = readInt(from) != 0u;
      readInt(from);  // obsolete verbosity
      settings.maxBuildJobs.assign(readInt(from));
      settings.maxSilentTime = readInt(from);
      readInt(from);  // obsolete useBuildHook
      settings.verboseBuild = 0 == readInt(from);
      readInt(from);  // obsolete logType
      readInt(from);  // obsolete printBuildTrace
      settings.buildCores = readInt(from);
      settings.useSubstitutes = readInt(from) != 0u;

      StringMap overrides;
      if (GET_PROTOCOL_MINOR(clientVersion) >= 12) {
        unsigned int n = readInt(from);
        for (unsigned int i = 0; i < n; i++) {
          std::string name = readString(from);
          std::string value = readString(from);
          overrides.emplace(name, value);
        }
      }

      logger->startWork();

      for (auto& i : overrides) {
        auto& name(i.first);
        auto& value(i.second);

        auto setSubstituters = [&](Setting<Strings>& res) {
          if (name != res.name && res.aliases.count(name) == 0) {
            return false;
          }
          StringSet trusted = settings.trustedSubstituters;
          for (auto& s : settings.substituters.get()) {
            trusted.insert(s);
          }
          Strings subs;
          Strings ss = absl::StrSplit(value, absl::ByAnyChar(" \t\n\r"));
          for (auto& s : ss) {
            if (trusted.count(s) != 0u) {
              subs.push_back(s);
            } else {
              LOG(WARNING) << "ignoring untrusted substituter '" << s << "'";
            }
          }
          res = subs;
          return true;
        };

        try {
          if (name == "ssh-auth-sock") {  // obsolete
            ;
          } else if (trusted || name == settings.buildTimeout.name ||
                     name == "connect-timeout" ||
                     (name == "builders" && value.empty())) {
            settings.set(name, value);
          } else if (setSubstituters(settings.substituters)) {
            ;
          } else if (setSubstituters(settings.extraSubstituters)) {
            ;
          } else {
            LOG(WARNING) << "ignoring the user-specified setting '" << name
                         << "', because it is a "
                         << "restricted setting and you are not a trusted user";
          }
        } catch (UsageError& e) {
          LOG(WARNING) << e.what();
        }
      }

      logger->stopWork();
      break;
    }

    case wopQuerySubstitutablePathInfo: {
      Path path = absPath(readString(from));
      logger->startWork();
      SubstitutablePathInfos infos;
      store->querySubstitutablePathInfos({path}, infos);
      logger->stopWork();
      auto i = infos.find(path);
      if (i == infos.end()) {
        to << 0;
      } else {
        to << 1 << i->second.deriver << i->second.references
           << i->second.downloadSize << i->second.narSize;
      }
      break;
    }

    case wopQuerySubstitutablePathInfos: {
      auto paths = readStorePaths<PathSet>(*store, from);
      logger->startWork();
      SubstitutablePathInfos infos;
      store->querySubstitutablePathInfos(paths, infos);
      logger->stopWork();
      to << infos.size();
      for (auto& i : infos) {
        to << i.first << i.second.deriver << i.second.references
           << i.second.downloadSize << i.second.narSize;
      }
      break;
    }

    case wopQueryAllValidPaths: {
      logger->startWork();
      PathSet paths = store->queryAllValidPaths();
      logger->stopWork();
      to << paths;
      break;
    }

    case wopQueryPathInfo: {
      Path path = readStorePath(*store, from);
      std::shared_ptr<const ValidPathInfo> info;
      logger->startWork();
      try {
        info = store->queryPathInfo(path);
      } catch (InvalidPath&) {
        if (GET_PROTOCOL_MINOR(clientVersion) < 17) {
          throw;
        }
      }
      logger->stopWork();
      if (info) {
        if (GET_PROTOCOL_MINOR(clientVersion) >= 17) {
          to << 1;
        }
        to << info->deriver << info->narHash.to_string(Base16, false)
           << info->references << info->registrationTime << info->narSize;
        if (GET_PROTOCOL_MINOR(clientVersion) >= 16) {
          to << static_cast<uint64_t>(info->ultimate) << info->sigs << info->ca;
        }
      } else {
        assert(GET_PROTOCOL_MINOR(clientVersion) >= 17);
        to << 0;
      }
      break;
    }

    case wopOptimiseStore: {
      logger->startWork();
      store->optimiseStore();
      logger->stopWork();
      to << 1;
      break;
    }

    case wopVerifyStore: {
      bool checkContents;
      bool repair;
      from >> checkContents >> repair;
      logger->startWork();
      if (repair && !trusted) {
        throw Error("you are not privileged to repair paths");
      }
      bool errors = store->verifyStore(checkContents, (RepairFlag)repair);
      logger->stopWork();
      to << static_cast<uint64_t>(errors);
      break;
    }

    case wopAddSignatures: {
      Path path = readStorePath(*store, from);
      auto sigs = readStrings<StringSet>(from);
      logger->startWork();
      if (!trusted) {
        throw Error("you are not privileged to add signatures");
      }
      store->addSignatures(path, sigs);
      logger->stopWork();
      to << 1;
      break;
    }

    case wopNarFromPath: {
      auto path = readStorePath(*store, from);
      logger->startWork();
      logger->stopWork();
      dumpPath(path, to);
      break;
    }

    case wopAddToStoreNar: {
      bool repair;
      bool dontCheckSigs;
      ValidPathInfo info;
      info.path = readStorePath(*store, from);
      from >> info.deriver;
      if (!info.deriver.empty()) {
        store->assertStorePath(info.deriver);
      }
      info.narHash = Hash(readString(from), htSHA256);
      info.references = readStorePaths<PathSet>(*store, from);
      from >> info.registrationTime >> info.narSize >> info.ultimate;
      info.sigs = readStrings<StringSet>(from);
      from >> info.ca >> repair >> dontCheckSigs;
      if (!trusted && dontCheckSigs) {
        dontCheckSigs = false;
      }
      if (!trusted) {
        info.ultimate = false;
      }

      std::string saved;
      std::unique_ptr<Source> source;
      if (GET_PROTOCOL_MINOR(clientVersion) >= 21) {
        source = std::make_unique<TunnelSource>(from);
      } else {
        TeeSink tee(from);
        parseDump(tee, tee.source);
        saved = std::move(*tee.source.data);
        source = std::make_unique<StringSource>(saved);
      }

      logger->startWork();

      // FIXME: race if addToStore doesn't read source?
      store->addToStore(info, *source, (RepairFlag)repair,
                        dontCheckSigs ? NoCheckSigs : CheckSigs, nullptr);

      logger->stopWork();
      break;
    }

    case wopQueryMissing: {
      auto targets = readStorePaths<PathSet>(*store, from);
      logger->startWork();
      PathSet willBuild;
      PathSet willSubstitute;
      PathSet unknown;
      unsigned long long downloadSize;
      unsigned long long narSize;
      store->queryMissing(targets, willBuild, willSubstitute, unknown,
                          downloadSize, narSize);
      logger->stopWork();
      to << willBuild << willSubstitute << unknown << downloadSize << narSize;
      break;
    }

    default:
      throw Error(format("invalid operation %1%") % op);
  }
}

static void processConnection(bool trusted, const std::string& userName,
                              uid_t userId) {
  MonitorFdHup monitor(from.fd);

  /* Exchange the greeting. */
  unsigned int magic = readInt(from);
  if (magic != WORKER_MAGIC_1) {
    throw Error("protocol mismatch");
  }
  to << WORKER_MAGIC_2 << PROTOCOL_VERSION;
  to.flush();
  unsigned int clientVersion = readInt(from);

  if (clientVersion < 0x10a) {
    throw Error("the Nix client version is too old");
  }

  auto tunnelLogger = new TunnelLogger(clientVersion);
  // logger = tunnelLogger;

  unsigned int opCount = 0;

  Finally finally([&]() {
    _isInterrupted = false;
    DLOG(INFO) << opCount << " operations";
  });

  if (GET_PROTOCOL_MINOR(clientVersion) >= 14 && (readInt(from) != 0u)) {
    setAffinityTo(readInt(from));
  }

  readInt(from);  // obsolete reserveSpace

  /* Send startup error messages to the client. */
  tunnelLogger->startWork();

  try {
    /* If we can't accept clientVersion, then throw an error
     *here* (not above). */

#if 0
        /* Prevent users from doing something very dangerous. */
        if (geteuid() == 0 &&
            querySetting("build-users-group", "") == "")
            throw Error("if you run 'nix-daemon' as root, then you MUST set 'build-users-group'!");
#endif

    /* Open the store. */
    Store::Params params;  // FIXME: get params from somewhere
    // Disable caching since the client already does that.
    params["path-info-cache-size"] = "0";
    auto store = openStore(settings.storeUri, params);

    store->createUser(userName, userId);

    tunnelLogger->stopWork();
    to.flush();

    /* Process client requests. */
    while (true) {
      WorkerOp op;
      try {
        op = (WorkerOp)readInt(from);
      } catch (Interrupted& e) {
        break;
      } catch (EndOfFile& e) {
        break;
      }

      opCount++;

      try {
        performOp(tunnelLogger, store, trusted, clientVersion, from, to, op);
      } catch (Error& e) {
        /* If we're not in a state where we can send replies, then
           something went wrong processing the input of the
           client.  This can happen especially if I/O errors occur
           during addTextToStore() / importPath().  If that
           happens, just send the error message and exit. */
        bool errorAllowed = tunnelLogger->state_.lock()->canSendStderr;
        tunnelLogger->stopWork(false, e.msg(), e.status);
        if (!errorAllowed) {
          throw;
        }
      } catch (std::bad_alloc& e) {
        tunnelLogger->stopWork(false, "Nix daemon out of memory", 1);
        throw;
      }

      to.flush();

      assert(!tunnelLogger->state_.lock()->canSendStderr);
    };

  } catch (std::exception& e) {
    tunnelLogger->stopWork(false, e.what(), 1);
    to.flush();
    return;
  }
}

static void sigChldHandler(int sigNo) {
  // Ensure we don't modify errno of whatever we've interrupted
  auto saved_errno = errno;
  /* Reap all dead children. */
  while (waitpid(-1, nullptr, WNOHANG) > 0) {
    ;
  }
  errno = saved_errno;
}

static void setSigChldAction(bool autoReap) {
  struct sigaction act;
  struct sigaction oact;
  act.sa_handler = autoReap ? sigChldHandler : SIG_DFL;
  sigfillset(&act.sa_mask);
  act.sa_flags = 0;
  if (sigaction(SIGCHLD, &act, &oact) != 0) {
    throw SysError("setting SIGCHLD handler");
  }
}

bool matchUser(const std::string& user, const std::string& group,
               const Strings& users) {
  if (find(users.begin(), users.end(), "*") != users.end()) {
    return true;
  }

  if (find(users.begin(), users.end(), user) != users.end()) {
    return true;
  }

  for (auto& i : users) {
    if (std::string(i, 0, 1) == "@") {
      if (group == std::string(i, 1)) {
        return true;
      }
      struct group* gr = getgrnam(i.c_str() + 1);
      if (gr == nullptr) {
        continue;
      }
      for (char** mem = gr->gr_mem; *mem != nullptr; mem++) {
        if (user == std::string(*mem)) {
          return true;
        }
      }
    }
  }

  return false;
}

struct PeerInfo {
  bool pidKnown;
  pid_t pid;
  bool uidKnown;
  uid_t uid;
  bool gidKnown;
  gid_t gid;
};

/* Get the identity of the caller, if possible. */
static PeerInfo getPeerInfo(int remote) {
  PeerInfo peer = {false, 0, false, 0, false, 0};

#if defined(SO_PEERCRED)

  ucred cred;
  socklen_t credLen = sizeof(cred);
  if (getsockopt(remote, SOL_SOCKET, SO_PEERCRED, &cred, &credLen) == -1) {
    throw SysError("getting peer credentials");
  }
  peer = {true, cred.pid, true, cred.uid, true, cred.gid};

#elif defined(LOCAL_PEERCRED)

#if !defined(SOL_LOCAL)
#define SOL_LOCAL 0
#endif

  xucred cred;
  socklen_t credLen = sizeof(cred);
  if (getsockopt(remote, SOL_LOCAL, LOCAL_PEERCRED, &cred, &credLen) == -1)
    throw SysError("getting peer credentials");
  peer = {false, 0, true, cred.cr_uid, false, 0};

#endif

  return peer;
}

#define SD_LISTEN_FDS_START 3

static void daemonLoop(char** argv) {
  if (chdir("/") == -1) {
    throw SysError("cannot change current directory");
  }

  /* Get rid of children automatically; don't let them become
     zombies. */
  setSigChldAction(true);

  AutoCloseFD fdSocket;

  /* Handle socket-based activation by systemd. */
  if (!getEnv("LISTEN_FDS").empty()) {
    if (getEnv("LISTEN_PID") != std::to_string(getpid()) ||
        getEnv("LISTEN_FDS") != "1") {
      throw Error("unexpected systemd environment variables");
    }
    fdSocket = SD_LISTEN_FDS_START;
  }

  /* Otherwise, create and bind to a Unix domain socket. */
  else {
    /* Create and bind to a Unix domain socket. */
    fdSocket = socket(PF_UNIX, SOCK_STREAM, 0);
    if (!fdSocket) {
      throw SysError("cannot create Unix domain socket");
    }

    std::string socketPath = settings.nixDaemonSocketFile;

    createDirs(dirOf(socketPath));

    /* Urgh, sockaddr_un allows path names of only 108 characters.
       So chdir to the socket directory so that we can pass a
       relative path name. */
    if (chdir(dirOf(socketPath).c_str()) == -1) {
      throw SysError("cannot change current directory");
    }
    Path socketPathRel = "./" + baseNameOf(socketPath);

    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    if (socketPathRel.size() >= sizeof(addr.sun_path)) {
      throw Error(format("socket path '%1%' is too long") % socketPathRel);
    }
    strcpy(addr.sun_path, socketPathRel.c_str());

    unlink(socketPath.c_str());

    /* Make sure that the socket is created with 0666 permission
       (everybody can connect --- provided they have access to the
       directory containing the socket). */
    mode_t oldMode = umask(0111);
    int res = bind(fdSocket.get(), (struct sockaddr*)&addr, sizeof(addr));
    umask(oldMode);
    if (res == -1) {
      throw SysError(format("cannot bind to socket '%1%'") % socketPath);
    }

    if (chdir("/") == -1) { /* back to the root */
      throw SysError("cannot change current directory");
    }

    if (listen(fdSocket.get(), 5) == -1) {
      throw SysError(format("cannot listen on socket '%1%'") % socketPath);
    }
  }

  closeOnExec(fdSocket.get());

  /* Loop accepting connections. */
  while (true) {
    try {
      /* Accept a connection. */
      struct sockaddr_un remoteAddr;
      socklen_t remoteAddrLen = sizeof(remoteAddr);

      AutoCloseFD remote =
          accept(fdSocket.get(), (struct sockaddr*)&remoteAddr, &remoteAddrLen);
      checkInterrupt();
      if (!remote) {
        if (errno == EINTR) {
          continue;
        }
        throw SysError("accepting connection");
      }

      closeOnExec(remote.get());

      bool trusted = false;
      PeerInfo peer = getPeerInfo(remote.get());

      struct passwd* pw = peer.uidKnown ? getpwuid(peer.uid) : nullptr;
      std::string user = pw != nullptr ? pw->pw_name : std::to_string(peer.uid);

      struct group* gr = peer.gidKnown ? getgrgid(peer.gid) : nullptr;
      std::string group =
          gr != nullptr ? gr->gr_name : std::to_string(peer.gid);

      Strings trustedUsers = settings.trustedUsers;
      Strings allowedUsers = settings.allowedUsers;

      if (matchUser(user, group, trustedUsers)) {
        trusted = true;
      }

      if ((!trusted && !matchUser(user, group, allowedUsers)) ||
          group == settings.buildUsersGroup) {
        throw Error(
            format("user '%1%' is not allowed to connect to the Nix daemon") %
            user);
      }

      LOG(INFO) << "accepted connection from pid "
                << (peer.pidKnown ? std::to_string(peer.pid) : "<unknown>")
                << ", user " << (peer.uidKnown ? user : "<unknown>")
                << (trusted ? " (trusted)" : "");

      /* Fork a child to handle the connection. */
      ProcessOptions options;
      options.errorPrefix = "unexpected Nix daemon error: ";
      options.dieWithParent = false;
      options.runExitHandlers = true;
      options.allowVfork = false;
      startProcess(
          [&]() {
            fdSocket = -1;

            /* Background the daemon. */
            if (setsid() == -1) {
              throw SysError(format("creating a new session"));
            }

            /* Restore normal handling of SIGCHLD. */
            setSigChldAction(false);

            /* For debugging, stuff the pid into argv[1]. */
            if (peer.pidKnown && (argv[1] != nullptr)) {
              std::string processName = std::to_string(peer.pid);
              strncpy(argv[1], processName.c_str(), strlen(argv[1]));
            }

            /* Handle the connection. */
            from.fd = remote.get();
            to.fd = remote.get();
            processConnection(trusted, user, peer.uid);

            exit(0);
          },
          options);

    } catch (Interrupted& e) {
      return;
    } catch (Error& e) {
      LOG(ERROR) << "error processing connection: " << e.msg();
    }
  }
}

static int _main(int argc, char** argv) {
  {
    auto stdio = false;

    parseCmdLine(argc, argv,
                 [&](Strings::iterator& arg, const Strings::iterator& end) {
                   if (*arg == "--daemon") {
                     ; /* ignored for backwards compatibility */
                   } else if (*arg == "--help") {
                     showManPage("nix-daemon");
                   } else if (*arg == "--version") {
                     printVersion("nix-daemon");
                   } else if (*arg == "--stdio") {
                     stdio = true;
                   } else {
                     return false;
                   }
                   return true;
                 });

    if (stdio) {
      if (getStoreType() == tDaemon) {
        /* Forward on this connection to the real daemon */
        auto socketPath = settings.nixDaemonSocketFile;
        auto s = socket(PF_UNIX, SOCK_STREAM, 0);
        if (s == -1) {
          throw SysError("creating Unix domain socket");
        }

        auto socketDir = dirOf(socketPath);
        if (chdir(socketDir.c_str()) == -1) {
          throw SysError(format("changing to socket directory '%1%'") %
                         socketDir);
        }

        auto socketName = baseNameOf(socketPath);
        auto addr = sockaddr_un{};
        addr.sun_family = AF_UNIX;
        if (socketName.size() + 1 >= sizeof(addr.sun_path)) {
          throw Error(format("socket name %1% is too long") % socketName);
        }
        strcpy(addr.sun_path, socketName.c_str());

        if (connect(s, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
          throw SysError(format("cannot connect to daemon at %1%") %
                         socketPath);
        }

        auto nfds = (s > STDIN_FILENO ? s : STDIN_FILENO) + 1;
        while (true) {
          fd_set fds;
          FD_ZERO(&fds);
          FD_SET(s, &fds);
          FD_SET(STDIN_FILENO, &fds);
          if (select(nfds, &fds, nullptr, nullptr, nullptr) == -1) {
            throw SysError("waiting for data from client or server");
          }
          if (FD_ISSET(s, &fds)) {
            auto res = splice(s, nullptr, STDOUT_FILENO, nullptr, SSIZE_MAX,
                              SPLICE_F_MOVE);
            if (res == -1) {
              throw SysError("splicing data from daemon socket to stdout");
            }
            if (res == 0) {
              throw EndOfFile("unexpected EOF from daemon socket");
            }
          }
          if (FD_ISSET(STDIN_FILENO, &fds)) {
            auto res = splice(STDIN_FILENO, nullptr, s, nullptr, SSIZE_MAX,
                              SPLICE_F_MOVE);
            if (res == -1) {
              throw SysError("splicing data from stdin to daemon socket");
            }
            if (res == 0) {
              return 0;
            }
          }
        }
      } else {
        processConnection(true, "root", 0);
      }
    } else {
      daemonLoop(argv);
    }

    return 0;
  }
}

static RegisterLegacyCommand s1("nix-daemon", _main);
