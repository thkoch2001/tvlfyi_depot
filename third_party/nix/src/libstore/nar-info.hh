#pragma once

#include "libstore/store-api.hh"
#include "libutil/hash.hh"
#include "libutil/types.hh"

namespace nix {

struct NarInfo : ValidPathInfo {
  std::string url;
  std::string compression;
  Hash fileHash;
  uint64_t fileSize = 0;
  std::string system;

  NarInfo() {}
  NarInfo(const ValidPathInfo& info) : ValidPathInfo(info) {}
  NarInfo(const Store& store, const std::string& s, const std::string& whence);

  std::string to_string() const;
};

}  // namespace nix
