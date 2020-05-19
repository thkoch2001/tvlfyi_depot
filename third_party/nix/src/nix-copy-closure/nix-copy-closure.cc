#include <glog/logging.h>
#include "legacy.hh"
#include "shared.hh"
#include "store-api.hh"

using namespace nix;

static int _main(int argc, char** argv) {
  {
    auto gzip = false;
    auto toMode = true;
    auto includeOutputs = false;
    auto dryRun = false;
    auto useSubstitutes = NoSubstitute;
    std::string sshHost;
    PathSet storePaths;

    parseCmdLine(
        argc, argv, [&](Strings::iterator& arg, const Strings::iterator& end) {
          if (*arg == "--help")
            showManPage("nix-copy-closure");
          else if (*arg == "--version")
            printVersion("nix-copy-closure");
          else if (*arg == "--gzip" || *arg == "--bzip2" || *arg == "--xz") {
            if (*arg != "--gzip") {
              LOG(WARNING) << "'" << *arg
                           << "' is not implemented, falling back to gzip";
            }
            gzip = true;
          } else if (*arg == "--from")
            toMode = false;
          else if (*arg == "--to")
            toMode = true;
          else if (*arg == "--include-outputs")
            includeOutputs = true;
          else if (*arg == "--show-progress")
            LOG(WARNING) << "'--show-progress' is not implemented";
          else if (*arg == "--dry-run")
            dryRun = true;
          else if (*arg == "--use-substitutes" || *arg == "-s")
            useSubstitutes = Substitute;
          else if (sshHost.empty())
            sshHost = *arg;
          else
            storePaths.insert(*arg);
          return true;
        });

    initPlugins();

    if (sshHost.empty()) throw UsageError("no host name specified");

    auto remoteUri = "ssh://" + sshHost + (gzip ? "?compress=true" : "");
    auto to = toMode ? openStore(remoteUri) : openStore();
    auto from = toMode ? openStore() : openStore(remoteUri);

    PathSet storePaths2;
    for (auto& path : storePaths)
      storePaths2.insert(from->followLinksToStorePath(path));

    PathSet closure;
    from->computeFSClosure(storePaths2, closure, false, includeOutputs);

    copyPaths(from, to, closure, NoRepair, NoCheckSigs, useSubstitutes);

    return 0;
  }
}

static RegisterLegacyCommand s1("nix-copy-closure", _main);
