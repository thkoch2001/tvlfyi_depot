#include <glog/logging.h>

#include "libmain/common-args.hh"
#include "libmain/shared.hh"
#include "libstore/store-api.hh"
#include "nix/command.hh"

using namespace nix;

struct CmdLog final : InstallableCommand {
  CmdLog() = default;

  std::string name() override { return "log"; }

  std::string description() override {
    return "show the build log of the specified packages or paths, if "
           "available";
  }

  Examples examples() override {
    return {
        Example{"To get the build log of GNU Hello:", "nix log nixpkgs.hello"},
        Example{
            "To get the build log of a specific path:",
            "nix log "
            "/nix/store/lmngj4wcm9rkv3w4dfhzhcyij3195hiq-thunderbird-52.2.1"},
        Example{"To get a build log from a specific binary cache:",
                "nix log --store https://cache.nixos.org nixpkgs.hello"},
    };
  }

  void run(ref<Store> store) override {
    settings.readOnlyMode = true;

    auto subs = getDefaultSubstituters();

    subs.push_front(store);

    auto b = installable->toBuildable();

    RunPager pager;
    for (auto& sub : subs) {
      auto log = !b.drvPath.empty() ? sub->getBuildLog(b.drvPath) : nullptr;
      for (auto& output : b.outputs) {
        if (log) {
          break;
        }
        log = sub->getBuildLog(output.second);
      }
      if (!log) {
        continue;
      }
      LOG(INFO) << "got build log for '" << installable->what() << "' from '"
                << sub->getUri() << "'";
      std::cout << *log;
      return;
    }

    throw Error("build log of '%s' is not available", installable->what());
  }
};

static RegisterCommand r1(make_ref<CmdLog>());
