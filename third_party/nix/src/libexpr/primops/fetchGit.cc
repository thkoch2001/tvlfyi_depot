#include <nlohmann/json.hpp>
#include <regex>

#include <absl/strings/ascii.h>
#include <absl/strings/match.h>
#include <absl/strings/str_split.h>
#include <glog/logging.h>
#include <sys/time.h>

#include "download.hh"
#include "eval-inline.hh"
#include "hash.hh"
#include "pathlocks.hh"
#include "primops.hh"
#include "store-api.hh"

using namespace std::string_literals;

namespace nix {

struct GitInfo {
  Path storePath;
  std::string rev;
  std::string shortRev;
  uint64_t revCount = 0;
};

std::regex revRegex("^[0-9a-fA-F]{40}$");

GitInfo exportGit(ref<Store> store, const std::string& uri,
                  std::optional<std::string> ref, std::string rev,
                  const std::string& name) {
  if (evalSettings.pureEval && rev == "")
    throw Error("in pure evaluation mode, 'fetchGit' requires a Git revision");

  if (!ref && rev == "" && absl::StartsWith(uri, "/") &&
      pathExists(uri + "/.git")) {
    bool clean = true;

    try {
      runProgram("git", true,
                 {"-C", uri, "diff-index", "--quiet", "HEAD", "--"});
    } catch (ExecError& e) {
      if (!WIFEXITED(e.status) || WEXITSTATUS(e.status) != 1) {
        throw;
      }
      clean = false;
    }

    if (!clean) {
      /* This is an unclean working tree. So copy all tracked
         files. */

      GitInfo gitInfo;
      gitInfo.rev = "0000000000000000000000000000000000000000";
      gitInfo.shortRev = std::string(gitInfo.rev, 0, 7);

      std::set<std::string> files =
          absl::StrSplit(runProgram("git", true, {"-C", uri, "ls-files", "-z"}),
                         absl::ByChar('\0'));

      PathFilter filter = [&](const Path& p) -> bool {
        assert(absl::StartsWith(p, uri));
        std::string file(p, uri.size() + 1);

        auto st = lstat(p);

        if (S_ISDIR(st.st_mode)) {
          auto prefix = file + "/";
          auto i = files.lower_bound(prefix);
          return i != files.end() && absl::StartsWith(*i, prefix);
        }

        return files.count(file);
      };

      gitInfo.storePath =
          store->addToStore("source", uri, true, htSHA256, filter);

      return gitInfo;
    }

    // clean working tree, but no ref or rev specified.  Use 'HEAD'.
    rev = absl::StripTrailingAsciiWhitespace(
        runProgram("git", true, {"-C", uri, "rev-parse", "HEAD"}));
    ref = "HEAD"s;
  }

  if (!ref) {
    ref = "HEAD"s;
  }

  if (rev != "" && !std::regex_match(rev, revRegex))
    throw Error("invalid Git revision '%s'", rev);

  deletePath(getCacheDir() + "/nix/git");

  Path cacheDir = getCacheDir() + "/nix/gitv2/" +
                  hashString(htSHA256, uri).to_string(Base32, false);

  if (!pathExists(cacheDir)) {
    createDirs(dirOf(cacheDir));
    runProgram("git", true, {"init", "--bare", cacheDir});
  }

  Path localRefFile;
  if (ref->compare(0, 5, "refs/") == 0)
    localRefFile = cacheDir + "/" + *ref;
  else
    localRefFile = cacheDir + "/refs/heads/" + *ref;

  bool doFetch;
  time_t now = time(0);
  /* If a rev was specified, we need to fetch if it's not in the
     repo. */
  if (rev != "") {
    try {
      runProgram("git", true, {"-C", cacheDir, "cat-file", "-e", rev});
      doFetch = false;
    } catch (ExecError& e) {
      if (WIFEXITED(e.status)) {
        doFetch = true;
      } else {
        throw;
      }
    }
  } else {
    /* If the local ref is older than ‘tarball-ttl’ seconds, do a
       git fetch to update the local ref to the remote ref. */
    struct stat st;
    doFetch = stat(localRefFile.c_str(), &st) != 0 ||
              (uint64_t)st.st_mtime + settings.tarballTtl <= (uint64_t)now;
  }
  if (doFetch) {
    DLOG(INFO) << "fetching Git repository '" << uri << "'";

    // FIXME: git stderr messes up our progress indicator, so
    // we're using --quiet for now. Should process its stderr.
    runProgram("git", true,
               {"-C", cacheDir, "fetch", "--quiet", "--force", "--", uri,
                fmt("%s:%s", *ref, *ref)});

    struct timeval times[2];
    times[0].tv_sec = now;
    times[0].tv_usec = 0;
    times[1].tv_sec = now;
    times[1].tv_usec = 0;

    utimes(localRefFile.c_str(), times);
  }

  // FIXME: check whether rev is an ancestor of ref.
  GitInfo gitInfo;
  gitInfo.rev =
      rev != "" ? rev
                : absl::StripTrailingAsciiWhitespace(readFile(localRefFile));
  gitInfo.shortRev = std::string(gitInfo.rev, 0, 7);

  DLOG(INFO) << "using revision " << gitInfo.rev << " of repo '" << uri << "'";

  std::string storeLinkName =
      hashString(htSHA512, name + std::string("\0"s) + gitInfo.rev)
          .to_string(Base32, false);
  Path storeLink = cacheDir + "/" + storeLinkName + ".link";
  PathLocks storeLinkLock({storeLink}, fmt("waiting for lock on '%1%'...",
                                           storeLink));  // FIXME: broken

  try {
    auto json = nlohmann::json::parse(readFile(storeLink));

    assert(json["name"] == name && json["rev"] == gitInfo.rev);

    gitInfo.storePath = json["storePath"];

    if (store->isValidPath(gitInfo.storePath)) {
      gitInfo.revCount = json["revCount"];
      return gitInfo;
    }

  } catch (SysError& e) {
    if (e.errNo != ENOENT) {
      throw;
    }
  }

  // FIXME: should pipe this, or find some better way to extract a
  // revision.
  auto tar = runProgram("git", true, {"-C", cacheDir, "archive", gitInfo.rev});

  Path tmpDir = createTempDir();
  AutoDelete delTmpDir(tmpDir, true);

  runProgram("tar", true, {"x", "-C", tmpDir}, tar);

  gitInfo.storePath = store->addToStore(name, tmpDir);

  gitInfo.revCount = std::stoull(runProgram(
      "git", true, {"-C", cacheDir, "rev-list", "--count", gitInfo.rev}));

  nlohmann::json json;
  json["storePath"] = gitInfo.storePath;
  json["uri"] = uri;
  json["name"] = name;
  json["rev"] = gitInfo.rev;
  json["revCount"] = gitInfo.revCount;

  writeFile(storeLink, json.dump());

  return gitInfo;
}

static void prim_fetchGit(EvalState& state, const Pos& pos, Value** args,
                          Value& v) {
  std::string url;
  std::optional<std::string> ref;
  std::string rev;
  std::string name = "source";
  PathSet context;

  state.forceValue(*args[0]);

  if (args[0]->type == tAttrs) {
    state.forceAttrs(*args[0], pos);

    for (auto& attr_iter : *args[0]->attrs) {
      auto& attr = attr_iter.second;
      std::string n(attr.name);
      if (n == "url")
        url =
            state.coerceToString(*attr.pos, *attr.value, context, false, false);
      else if (n == "ref")
        ref = state.forceStringNoCtx(*attr.value, *attr.pos);
      else if (n == "rev")
        rev = state.forceStringNoCtx(*attr.value, *attr.pos);
      else if (n == "name")
        name = state.forceStringNoCtx(*attr.value, *attr.pos);
      else
        throw EvalError("unsupported argument '%s' to 'fetchGit', at %s",
                        attr.name, *attr.pos);
    }

    if (url.empty())
      throw EvalError(format("'url' argument required, at %1%") % pos);

  } else {
    url = state.coerceToString(pos, *args[0], context, false, false);
  }

  // FIXME: git externals probably can be used to bypass the URI
  // whitelist. Ah well.
  state.checkURI(url);

  auto gitInfo = exportGit(state.store, url, ref, rev, name);

  state.mkAttrs(v, 8);
  mkString(*state.allocAttr(v, state.sOutPath), gitInfo.storePath,
           PathSet({gitInfo.storePath}));
  mkString(*state.allocAttr(v, state.symbols.Create("rev")), gitInfo.rev);
  mkString(*state.allocAttr(v, state.symbols.Create("shortRev")),
           gitInfo.shortRev);
  mkInt(*state.allocAttr(v, state.symbols.Create("revCount")),
        gitInfo.revCount);

  if (state.allowedPaths) {
    state.allowedPaths->insert(state.store->toRealPath(gitInfo.storePath));
  }
}

static RegisterPrimOp r("fetchGit", 1, prim_fetchGit);

}  // namespace nix
