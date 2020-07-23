#pragma once

#include "libstore/fs-accessor.hh"
#include "libstore/store-api.hh"
#include "libutil/ref.hh"

namespace nix {

class RemoteFSAccessor : public FSAccessor {
  std::shared_ptr<Store> store;

  std::map<Path, ref<FSAccessor>> nars;

  Path cacheDir;

  std::pair<ref<FSAccessor>, Path> fetch(const Path& path_);

  friend class BinaryCacheStore;

  Path makeCacheFile(const Path& storePath, const std::string& ext);

  void addToCache(const Path& storePath, const std::string& nar,
                  const ref<FSAccessor>& narAccessor);

 public:
  RemoteFSAccessor(const std::shared_ptr<Store>& store,
                   const /* FIXME: use std::optional */ Path& cacheDir = "");

  Stat stat(const Path& path) override;

  StringSet readDirectory(const Path& path) override;

  std::string readFile(const Path& path) override;

  std::string readLink(const Path& path) override;
};

}  // namespace nix
