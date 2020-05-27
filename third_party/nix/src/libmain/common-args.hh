#pragma once

#include "libutil/args.hh"

namespace nix {

struct MixCommonArgs : virtual Args {
  std::string programName;
  MixCommonArgs(const std::string& programName);
};

struct MixDryRun : virtual Args {
  bool dryRun = false;

  MixDryRun() {
    mkFlag(0, "dry-run", "show what this command would do without doing it",
           &dryRun);
  }
};

struct MixJSON : virtual Args {
  bool json = false;

  MixJSON() { mkFlag(0, "json", "produce JSON output", &json); }
};

}  // namespace nix
