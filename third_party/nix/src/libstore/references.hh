#pragma once

#include "libutil/hash.hh"
#include "libutil/types.hh"

namespace nix {

PathSet scanForReferences(const Path& path, const PathSet& refs,
                          HashResult& hash);

}
