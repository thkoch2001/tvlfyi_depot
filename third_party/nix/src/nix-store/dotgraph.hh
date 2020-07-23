#pragma once

#include "libutil/types.hh"

namespace nix {

class Store;

void printDotGraph(const std::shared_ptr<Store>& store, const PathSet& roots);

}  // namespace nix
