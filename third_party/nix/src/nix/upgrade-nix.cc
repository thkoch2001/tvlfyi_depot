#include <glog/logging.h>

#include "attr-path.hh"
#include "command.hh"
#include "common-args.hh"
#include "download.hh"
#include "eval.hh"
#include "names.hh"
#include "store-api.hh"

using namespace nix;

struct CmdUpgradeNix : MixDryRun, StoreCommand {
  Path profileDir;
  std::string storePathsUrl =
      "https://github.com/NixOS/nixpkgs/raw/master/nixos/modules/installer/"
      "tools/nix-fallback-paths.nix";

  CmdUpgradeNix() {
    mkFlag()
        .longName("profile")
        .shortName('p')
        .labels({"profile-dir"})
        .description("the Nix profile to upgrade")
        .dest(&profileDir);

    mkFlag()
        .longName("nix-store-paths-url")
        .labels({"url"})
        .description(
            "URL of the file that contains the store paths of the latest Nix "
            "release")
        .dest(&storePathsUrl);
  }

  std::string name() override { return "upgrade-nix"; }

  std::string description() override {
    return "upgrade Nix to the latest stable version";
  }

  Examples examples() override {
    return {
        Example{"To upgrade Nix to the latest stable version:",
                "nix upgrade-nix"},
        Example{
            "To upgrade Nix in a specific profile:",
            "nix upgrade-nix -p /nix/var/nix/profiles/per-user/alice/profile"},
    };
  }

  void run(ref<Store> store) override {
    evalSettings.pureEval = true;

    if (profileDir.empty()) {
      profileDir = getProfileDir(store);
    }

    LOG(INFO) << "upgrading Nix in profile '" << profileDir << "'";

    Path storePath;
    {
      LOG(INFO) << "querying latest Nix version";
      storePath = getLatestNix(store);
    }

    auto version = DrvName(storePathToName(storePath)).version;

    if (dryRun) {
      LOG(ERROR) << "would upgrade to version " << version;
      return;
    }

    {
      LOG(INFO) << "downloading '" << storePath << "'...";
      store->ensurePath(storePath);
    }

    {
      LOG(INFO) << "verifying that '" << storePath << "' works...";
      auto program = storePath + "/bin/nix-env";
      auto s = runProgram(program, false, {"--version"});
      if (s.find("Nix") == std::string::npos) {
        throw Error("could not verify that '%s' works", program);
      }
    }

    {
      LOG(INFO) << "installing '" << storePath << "' into profile '"
                << profileDir << "'...";
      runProgram(settings.nixBinDir + "/nix-env", false,
                 {"--profile", profileDir, "-i", storePath, "--no-sandbox"});
    }

    LOG(INFO) << ANSI_GREEN << "upgrade to version " << version << " done"
              << ANSI_NORMAL;
  }

  /* Return the profile in which Nix is installed. */
  static Path getProfileDir(ref<Store> store) {
    Path where;

    for (auto& dir : tokenizeString<Strings>(getEnv("PATH"), ":")) {
      if (pathExists(dir + "/nix-env")) {
        where = dir;
        break;
      }
    }

    if (where.empty()) {
      throw Error(
          "couldn't figure out how Nix is installed, so I can't upgrade it");
    }

    LOG(INFO) << "found Nix in '" << where << "'";

    if (hasPrefix(where, "/run/current-system")) {
      throw Error("Nix on NixOS must be upgraded via 'nixos-rebuild'");
    }

    Path profileDir = dirOf(where);

    // Resolve profile to /nix/var/nix/profiles/<name> link.
    while (canonPath(profileDir).find("/profiles/") == std::string::npos &&
           isLink(profileDir)) {
      profileDir = readLink(profileDir);
    }

    LOG(INFO) << "found profile '" << profileDir << "'";

    Path userEnv = canonPath(profileDir, true);

    if (baseNameOf(where) != "bin" || !hasSuffix(userEnv, "user-environment")) {
      throw Error("directory '%s' does not appear to be part of a Nix profile",
                  where);
    }

    if (!store->isValidPath(userEnv)) {
      throw Error("directory '%s' is not in the Nix store", userEnv);
    }

    return profileDir;
  }

  /* Return the store path of the latest stable Nix. */
  Path getLatestNix(ref<Store> store) {
    // FIXME: use nixos.org?
    auto req = DownloadRequest(storePathsUrl);
    auto res = getDownloader()->download(req);

    auto state = std::make_unique<EvalState>(Strings(), store);
    auto v = state->allocValue();
    state->eval(state->parseExprFromString(*res.data, "/no-such-path"), *v);
    Bindings& bindings(*state->allocBindings(0));
    auto v2 = findAlongAttrPath(*state, settings.thisSystem, bindings, *v);

    return state->forceString(*v2);
  }
};

static RegisterCommand r1(make_ref<CmdUpgradeNix>());
