#include <atomic>

#include <glog/logging.h>

#include "command.hh"
#include "shared.hh"
#include "store-api.hh"
#include "sync.hh"
#include "thread-pool.hh"

using namespace nix;

struct CmdVerify : StorePathsCommand {
  bool noContents = false;
  bool noTrust = false;
  Strings substituterUris;
  size_t sigsNeeded = 0;

  CmdVerify() {
    mkFlag(0, "no-contents", "do not verify the contents of each store path",
           &noContents);
    mkFlag(0, "no-trust", "do not verify whether each store path is trusted",
           &noTrust);
    mkFlag()
        .longName("substituter")
        .shortName('s')
        .labels({"store-uri"})
        .description("use signatures from specified store")
        .arity(1)
        .handler([&](std::vector<std::string> ss) {
          substituterUris.push_back(ss[0]);
        });
    mkIntFlag('n', "sigs-needed",
              "require that each path has at least N valid signatures",
              &sigsNeeded);
  }

  std::string name() override { return "verify"; }

  std::string description() override {
    return "verify the integrity of store paths";
  }

  Examples examples() override {
    return {
        Example{"To verify the entire Nix store:", "nix verify --all"},
        Example{"To check whether each path in the closure of Firefox has at "
                "least 2 signatures:",
                "nix verify -r -n2 --no-contents $(type -p firefox)"},
    };
  }

  void run(ref<Store> store, Paths storePaths) override {
    std::vector<ref<Store>> substituters;
    for (auto& s : substituterUris) {
      substituters.push_back(openStore(s));
    }

    auto publicKeys = getDefaultPublicKeys();

    std::atomic<size_t> done{0};
    std::atomic<size_t> untrusted{0};
    std::atomic<size_t> corrupted{0};
    std::atomic<size_t> failed{0};
    std::atomic<size_t> active{0};

    ThreadPool pool;

    auto doPath = [&](const Path& storePath) {
      try {
        checkInterrupt();

        LOG(INFO) << "checking '" << storePath << "'";

        MaintainCount<std::atomic<size_t>> mcActive(active);

        auto info = store->queryPathInfo(storePath);

        if (!noContents) {
          HashSink sink(info->narHash.type);
          store->narFromPath(info->path, sink);

          auto hash = sink.finish();

          if (hash.first != info->narHash) {
            corrupted++;
            LOG(WARNING) << "path '" << info->path
                         << "' was modified! expected hash '"
                         << info->narHash.to_string() << "', got '"
                         << hash.first.to_string() << "'";
          }
        }

        if (!noTrust) {
          bool good = false;

          if (info->ultimate && (sigsNeeded == 0u)) {
            good = true;

          } else {
            StringSet sigsSeen;
            size_t actualSigsNeeded = std::max(sigsNeeded, (size_t)1);
            size_t validSigs = 0;

            auto doSigs = [&](const StringSet& sigs) {
              for (const auto& sig : sigs) {
                if (sigsSeen.count(sig) != 0u) {
                  continue;
                }
                sigsSeen.insert(sig);
                if (validSigs < ValidPathInfo::maxSigs &&
                    info->checkSignature(publicKeys, sig)) {
                  validSigs++;
                }
              }
            };

            if (info->isContentAddressed(*store)) {
              validSigs = ValidPathInfo::maxSigs;
            }

            doSigs(info->sigs);

            for (auto& store2 : substituters) {
              if (validSigs >= actualSigsNeeded) {
                break;
              }
              try {
                auto info2 = store2->queryPathInfo(info->path);
                if (info2->isContentAddressed(*store)) {
                  validSigs = ValidPathInfo::maxSigs;
                }
                doSigs(info2->sigs);
              } catch (InvalidPath&) {
              } catch (Error& e) {
                LOG(ERROR) << e.what();
              }
            }

            if (validSigs >= actualSigsNeeded) {
              good = true;
            }
          }

          if (!good) {
            untrusted++;
            LOG(WARNING) << "path '" << info->path << "' is untrusted";
          }
        }

        done++;

      } catch (Error& e) {
        LOG(ERROR) << e.what();
        failed++;
      }
    };

    for (auto& storePath : storePaths) {
      pool.enqueue(std::bind(doPath, storePath));
    }

    pool.process();

    throw Exit((corrupted != 0u ? 1 : 0) | (untrusted != 0u ? 2 : 0) |
               (failed != 0u ? 4 : 0));
  }
};

static RegisterCommand r1(make_ref<CmdVerify>());
