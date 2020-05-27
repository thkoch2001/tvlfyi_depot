#pragma once

#include "libutil/types.hh"

namespace nix {

class Store;

void printGraphML(const ref<Store>& store, const PathSet& roots);

}  // namespace nix
