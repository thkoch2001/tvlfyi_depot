#pragma once

#include "hash.hh"
#include "types.hh"

namespace nix {

PathSet scanForReferences(const Path& path, const PathSet& refs,
                          HashResult& hash);

}
