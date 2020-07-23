#include <atomic>

#include <glog/logging.h>

#include "libmain/shared.hh"
#include "libstore/store-api.hh"
#include "libutil/thread-pool.hh"
#include "nix/command.hh"

using namespace nix;

struct CmdCopySigs final : StorePathsCommand {
  Strings substituterUris;

  CmdCopySigs() {
    mkFlag()
        .longName("substituter")
        .shortName('s')
        .labels({"store-uri"})
        .description("use signatures from specified store")
        .arity(1)
        .handler([&](std::vector<std::string> ss) {
          substituterUris.push_back(ss[0]);
        });
  }

  std::string name() override { return "copy-sigs"; }

  std::string description() override {
    return "copy path signatures from substituters (like binary caches)";
  }

  void run(std::shared_ptr<Store> store, Paths storePaths) override {
    if (substituterUris.empty()) {
      throw UsageError("you must specify at least one substituter using '-s'");
    }

    // FIXME: factor out commonality with MixVerify.
    std::vector<std::shared_ptr<Store>> substituters;
    for (auto& s : substituterUris) {
      substituters.push_back(openStore(s));
    }

    ThreadPool pool;

    std::string doneLabel = "done";
    std::atomic<size_t> added{0};

    // logger->setExpected(doneLabel, storePaths.size());

    auto doPath = [&](const Path& storePath) {
      // Activity act(*logger, lvlInfo, format("getting signatures for '%s'") %
      // storePath);

      checkInterrupt();

      auto info = store->queryPathInfo(storePath);

      StringSet newSigs;

      for (auto& store2 : substituters) {
        try {
          auto info2 = store2->queryPathInfo(storePath);

          /* Don't import signatures that don't match this
             binary. */
          if (info->narHash != info2->narHash ||
              info->narSize != info2->narSize ||
              info->references != info2->references) {
            continue;
          }

          for (auto& sig : info2->sigs) {
            if (info->sigs.count(sig) == 0u) {
              newSigs.insert(sig);
            }
          }
        } catch (InvalidPath&) {
        }
      }

      if (!newSigs.empty()) {
        store->addSignatures(storePath, newSigs);
        added += newSigs.size();
      }

      // logger->incProgress(doneLabel);
    };

    for (auto& storePath : storePaths) {
      pool.enqueue(std::bind(doPath, storePath));
    }

    pool.process();

    LOG(INFO) << "imported " << added << " signatures";
  }
};

static RegisterCommand r1(make_ref<CmdCopySigs>());

struct CmdSignPaths final : StorePathsCommand {
  Path secretKeyFile;

  CmdSignPaths() {
    mkFlag()
        .shortName('k')
        .longName("key-file")
        .label("file")
        .description("file containing the secret signing key")
        .dest(&secretKeyFile);
  }

  std::string name() override { return "sign-paths"; }

  std::string description() override { return "sign the specified paths"; }

  void run(std::shared_ptr<Store> store, Paths storePaths) override {
    if (secretKeyFile.empty()) {
      throw UsageError("you must specify a secret key file using '-k'");
    }

    SecretKey secretKey(readFile(secretKeyFile));

    size_t added{0};

    for (auto& storePath : storePaths) {
      auto info = store->queryPathInfo(storePath);

      auto info2(*info);
      info2.sigs.clear();
      info2.sign(secretKey);
      assert(!info2.sigs.empty());

      if (info->sigs.count(*info2.sigs.begin()) == 0u) {
        store->addSignatures(storePath, info2.sigs);
        added++;
      }
    }

    LOG(INFO) << "added " << added << " signatures";
  }
};

static RegisterCommand r3(make_ref<CmdSignPaths>());
