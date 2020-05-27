#include <iostream>

#include <absl/strings/match.h>
#include <fcntl.h>
#include <glog/logging.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "libexpr/attr-path.hh"
#include "libexpr/common-eval-args.hh"
#include "libexpr/eval-inline.hh"
#include "libexpr/eval.hh"
#include "libmain/shared.hh"
#include "libstore/download.hh"
#include "libstore/store-api.hh"
#include "libutil/finally.hh"
#include "libutil/hash.hh"
#include "nix/legacy.hh"

using namespace nix;

/* If ‘uri’ starts with ‘mirror://’, then resolve it using the list of
   mirrors defined in Nixpkgs. */
std::string resolveMirrorUri(EvalState& state, std::string uri) {
  if (std::string(uri, 0, 9) != "mirror://") {
    return uri;
  }

  std::string s(uri, 9);
  auto p = s.find('/');
  if (p == std::string::npos) {
    throw Error("invalid mirror URI");
  }
  std::string mirrorName(s, 0, p);

  Value vMirrors;
  state.eval(
      state.parseExprFromString(
          "import <nixpkgs/pkgs/build-support/fetchurl/mirrors.nix>", "."),
      vMirrors);
  state.forceAttrs(vMirrors);

  auto mirrorList = vMirrors.attrs->find(state.symbols.Create(mirrorName));
  if (mirrorList == vMirrors.attrs->end()) {
    throw Error(format("unknown mirror name '%1%'") % mirrorName);
  }
  state.forceList(*mirrorList->second.value);

  if (mirrorList->second.value->listSize() < 1) {
    throw Error(format("mirror URI '%1%' did not expand to anything") % uri);
  }

  std::string mirror =
      state.forceString(*mirrorList->second.value->listElems()[0]);
  return mirror + (absl::EndsWith(mirror, "/") ? "" : "/") +
         std::string(s, p + 1);
}

static int _main(int argc, char** argv) {
  {
    HashType ht = htSHA256;
    std::vector<std::string> args;
    bool printPath = !getEnv("PRINT_PATH").empty();
    bool fromExpr = false;
    std::string attrPath;
    bool unpack = false;
    std::string name;

    struct MyArgs : LegacyArgs, MixEvalArgs {
      using LegacyArgs::LegacyArgs;
    };

    MyArgs myArgs(baseNameOf(argv[0]),
                  [&](Strings::iterator& arg, const Strings::iterator& end) {
                    if (*arg == "--help") {
                      showManPage("nix-prefetch-url");
                    } else if (*arg == "--version") {
                      printVersion("nix-prefetch-url");
                    } else if (*arg == "--type") {
                      std::string s = getArg(*arg, arg, end);
                      ht = parseHashType(s);
                      if (ht == htUnknown) {
                        throw UsageError(format("unknown hash type '%1%'") % s);
                      }
                    } else if (*arg == "--print-path") {
                      printPath = true;
                    } else if (*arg == "--attr" || *arg == "-A") {
                      fromExpr = true;
                      attrPath = getArg(*arg, arg, end);
                    } else if (*arg == "--unpack") {
                      unpack = true;
                    } else if (*arg == "--name") {
                      name = getArg(*arg, arg, end);
                    } else if (*arg != "" && arg->at(0) == '-') {
                      return false;
                    } else {
                      args.push_back(*arg);
                    }
                    return true;
                  });

    myArgs.parseCmdline(argvToStrings(argc, argv));

    initPlugins();

    if (args.size() > 2) {
      throw UsageError("too many arguments");
    }

    auto store = openStore();
    auto state = std::make_unique<EvalState>(myArgs.searchPath, store);

    Bindings& autoArgs = *myArgs.getAutoArgs(*state);

    /* If -A is given, get the URI from the specified Nix
       expression. */
    std::string uri;
    if (!fromExpr) {
      if (args.empty()) {
        throw UsageError("you must specify a URI");
      }
      uri = args[0];
    } else {
      Path path =
          resolveExprPath(lookupFileArg(*state, args.empty() ? "." : args[0]));
      Value vRoot;
      state->evalFile(path, vRoot);
      Value& v(*findAlongAttrPath(*state, attrPath, autoArgs, vRoot));
      state->forceAttrs(v);

      /* Extract the URI. */
      auto attr = v.attrs->find(state->symbols.Create("urls"));
      if (attr == v.attrs->end()) {
        throw Error("attribute set does not contain a 'urls' attribute");
      }
      state->forceList(*attr->second.value);
      if (attr->second.value->listSize() < 1) {
        throw Error("'urls' list is empty");
      }
      uri = state->forceString(*attr->second.value->listElems()[0]);

      /* Extract the hash mode. */
      attr = v.attrs->find(state->symbols.Create("outputHashMode"));
      if (attr == v.attrs->end()) {
        LOG(WARNING) << "this does not look like a fetchurl call";
      } else {
        unpack = state->forceString(*attr->second.value) == "recursive";
      }

      /* Extract the name. */
      if (name.empty()) {
        attr = v.attrs->find(state->symbols.Create("name"));
        if (attr != v.attrs->end()) {
          name = state->forceString(*attr->second.value);
        }
      }
    }

    /* Figure out a name in the Nix store. */
    if (name.empty()) {
      name = baseNameOf(uri);
    }
    if (name.empty()) {
      throw Error(format("cannot figure out file name for '%1%'") % uri);
    }

    /* If an expected hash is given, the file may already exist in
       the store. */
    Hash hash;
    Hash expectedHash(ht);
    Path storePath;
    if (args.size() == 2) {
      expectedHash = Hash(args[1], ht);
      storePath = store->makeFixedOutputPath(unpack, expectedHash, name);
      if (store->isValidPath(storePath)) {
        hash = expectedHash;
      } else {
        storePath.clear();
      }
    }

    if (storePath.empty()) {
      auto actualUri = resolveMirrorUri(*state, uri);

      AutoDelete tmpDir(createTempDir(), true);
      Path tmpFile = (Path)tmpDir + "/tmp";

      /* Download the file. */
      {
        AutoCloseFD fd =
            open(tmpFile.c_str(), O_WRONLY | O_CREAT | O_EXCL, 0600);
        if (!fd) {
          throw SysError("creating temporary file '%s'", tmpFile);
        }

        FdSink sink(fd.get());

        DownloadRequest req(actualUri);
        req.decompress = false;
        getDownloader()->download(std::move(req), sink);
      }

      /* Optionally unpack the file. */
      if (unpack) {
        LOG(INFO) << "unpacking...";
        Path unpacked = (Path)tmpDir + "/unpacked";
        createDirs(unpacked);
        if (absl::EndsWith(baseNameOf(uri), ".zip")) {
          runProgram("unzip", true, {"-qq", tmpFile, "-d", unpacked});
        } else {
          // FIXME: this requires GNU tar for decompression.
          runProgram("tar", true, {"xf", tmpFile, "-C", unpacked});
        }

        /* If the archive unpacks to a single file/directory, then use
           that as the top-level. */
        auto entries = readDirectory(unpacked);
        if (entries.size() == 1) {
          tmpFile = unpacked + "/" + entries[0].name;
        } else {
          tmpFile = unpacked;
        }
      }

      /* FIXME: inefficient; addToStore() will also hash
         this. */
      hash = unpack ? hashPath(ht, tmpFile).first : hashFile(ht, tmpFile);

      if (expectedHash != Hash(ht) && expectedHash != hash) {
        throw Error(format("hash mismatch for '%1%'") % uri);
      }

      /* Copy the file to the Nix store. FIXME: if RemoteStore
         implemented addToStoreFromDump() and downloadFile()
         supported a sink, we could stream the download directly
         into the Nix store. */
      storePath = store->addToStore(name, tmpFile, unpack, ht);

      assert(storePath == store->makeFixedOutputPath(unpack, hash, name));
    }

    if (!printPath) {
      LOG(INFO) << "path is '" << storePath << "'";
    }

    std::cout << printHash16or32(hash) << std::endl;
    if (printPath) {
      std::cout << storePath << std::endl;
    }

    return 0;
  }
}

static RegisterLegacyCommand s1("nix-prefetch-url", _main);
