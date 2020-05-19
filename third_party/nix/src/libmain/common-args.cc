#include "common-args.hh"

#include <glog/logging.h>

#include "globals.hh"

namespace nix {

MixCommonArgs::MixCommonArgs(const string& programName)
    : programName(programName) {
  mkFlag()
      .longName("option")
      .labels({"name", "value"})
      .description("set a Nix configuration option (overriding nix.conf)")
      .arity(2)
      .handler([](std::vector<std::string> ss) {
        try {
          globalConfig.set(ss[0], ss[1]);
        } catch (UsageError& e) {
          LOG(WARNING) << e.what();
        }
      });

  mkFlag()
      .longName("max-jobs")
      .shortName('j')
      .label("jobs")
      .description("maximum number of parallel builds")
      .handler([=](std::string s) { settings.set("max-jobs", s); });

  std::string cat = "config";
  globalConfig.convertToArgs(*this, cat);

  // Backward compatibility hack: nix-env already had a --system flag.
  if (programName == "nix-env") {
    longFlags.erase("system");
  }

  hiddenCategories.insert(cat);
}

}  // namespace nix
