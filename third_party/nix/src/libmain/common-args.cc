#include "libmain/common-args.hh"

#include <glog/logging.h>

#include "libstore/globals.hh"

namespace nix {

MixCommonArgs::MixCommonArgs(const std::string& programName)
    : programName(programName) {
  mkFlag()
      .longName("verbose")
      .shortName('v')
      .description("increase verbosity level")
      .handler([]() {
        if (FLAGS_stderrthreshold > 0) {
          FLAGS_stderrthreshold--;
        }
      });

  mkFlag()
      .longName("quiet")
      .description("decrease verbosity level")
      .handler([]() {
        if (FLAGS_stderrthreshold < google::GLOG_FATAL) {
          FLAGS_stderrthreshold++;
        }
      });

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
      .handler([=](const std::string& s) { settings.set("max-jobs", s); });

  std::string cat = "config";
  globalConfig.convertToArgs(*this, cat);

  // Backward compatibility hack: nix-env already had a --system flag.
  if (programName == "nix-env") {
    longFlags.erase("system");
  }

  hiddenCategories.insert(cat);
}

}  // namespace nix
