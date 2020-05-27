#pragma once

#include "libutil/types.hh"

namespace nix {

class Store;

void printDotGraph(const ref<Store>& store, const PathSet& roots);

}  // namespace nix
