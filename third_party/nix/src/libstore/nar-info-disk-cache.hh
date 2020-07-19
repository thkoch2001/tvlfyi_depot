#pragma once

#include "libstore/nar-info.hh"
#include "libutil/ref.hh"

namespace nix {

class NarInfoDiskCache {
 public:
  typedef enum { oValid, oInvalid, oUnknown } Outcome;

  virtual void createCache(const std::string& uri, const Path& storeDir,
                           bool wantMassQuery, int priority) = 0;

  virtual bool cacheExists(const std::string& uri, bool& wantMassQuery,
                           int& priority) = 0;

  virtual std::pair<Outcome, std::shared_ptr<NarInfo>> lookupNarInfo(
      const std::string& uri, const std::string& hashPart) = 0;

  virtual void upsertNarInfo(const std::string& uri,
                             const std::string& hashPart,
                             std::shared_ptr<ValidPathInfo> info) = 0;
};

/* Return a singleton cache object that can be used concurrently by
   multiple threads. */
std::shared_ptr<NarInfoDiskCache> getNarInfoDiskCache();

}  // namespace nix
