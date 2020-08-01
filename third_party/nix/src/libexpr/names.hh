#pragma once

#include <memory>
#include <regex>

#include "libutil/types.hh"

namespace nix {

struct DrvName {
  std::string fullName;
  std::string name;
  std::string version;
  unsigned int hits;

  DrvName();
  DrvName(const std::string& s);
  bool matches(DrvName& n);

 private:
  std::unique_ptr<std::regex> regex;
};

typedef std::list<DrvName> DrvNames;

std::string nextComponent(std::string::const_iterator& p,
                          const std::string::const_iterator end);
int compareVersions(const std::string& v1, const std::string& v2);
DrvNames drvNamesFromArgs(const Strings& opArgs);

}  // namespace nix
