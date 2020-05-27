#pragma once

#include "libstore/derivations.hh"

namespace nix {

// TODO: make pluggable.
void builtinFetchurl(const BasicDerivation& drv, const std::string& netrcData);
void builtinBuildenv(const BasicDerivation& drv);

}  // namespace nix
