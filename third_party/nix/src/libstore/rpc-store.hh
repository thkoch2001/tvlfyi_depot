#pragma once

#include <absl/strings/string_view.h>

#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"
#include "libstore/remote-store.hh"
#include "libstore/store-api.hh"

namespace nix::store {

// TODO(grfn): Currently, since the RPCStore is only used for the connection to
// the nix daemon over a unix socket, it inherits from the LocalFSStore since it
// shares a filesystem with the daemon. This will not always be the case, at
// which point we should tease these two things apart.
class RpcStore : public LocalFSStore, public virtual Store {
 public:
  RpcStore(const Params& params,
           std::unique_ptr<nix::proto::WorkerService::Stub> stub)
      : Store(params), LocalFSStore(params), stub_(std::move(stub)) {}

  RpcStore(std::string uri, const Params& params,
           std::unique_ptr<nix::proto::WorkerService::Stub> stub)
      : Store(params),
        LocalFSStore(params),
        uri_(uri),
        stub_(std::move(stub)) {}

  std::string getUri() override {
    if (uri_.has_value()) {
      return uri_.value();
    } else {
      return "daemon";
    }
  };

  virtual PathSet queryAllValidPaths() override;

  virtual void queryReferrers(const Path& path, PathSet& referrers) override;

  virtual PathSet queryValidDerivers(const Path& path) override;

  virtual PathSet queryDerivationOutputs(const Path& path) override;

  virtual StringSet queryDerivationOutputNames(const Path& path) override;

  virtual Path queryPathFromHashPart(const std::string& hashPart) override;

  virtual PathSet querySubstitutablePaths(const PathSet& paths) override;

  virtual void querySubstitutablePathInfos(
      const PathSet& paths, SubstitutablePathInfos& infos) override;

  virtual bool wantMassQuery() override { return true; }

  virtual void addToStore(const ValidPathInfo& info, Source& narSource,
                          RepairFlag repair = NoRepair,
                          CheckSigsFlag checkSigs = CheckSigs,
                          std::shared_ptr<FSAccessor> accessor = 0) override;

  virtual Path addToStore(const std::string& name, const Path& srcPath,
                          bool recursive = true, HashType hashAlgo = htSHA256,
                          PathFilter& filter = defaultPathFilter,
                          RepairFlag repair = NoRepair) override;

  virtual Path addTextToStore(const std::string& name, const std::string& s,
                              const PathSet& references,
                              RepairFlag repair = NoRepair) override;

  virtual absl::Status buildPaths(const PathSet& paths,
                          BuildMode buildMode = bmNormal) override;

  virtual BuildResult buildDerivation(const Path& drvPath,
                                      const BasicDerivation& drv,
                                      BuildMode buildMode = bmNormal) override;

  virtual void ensurePath(const Path& path) override;

  virtual void addTempRoot(const Path& path) override;

  virtual void addIndirectRoot(const Path& path) override;

  virtual void syncWithGC() override;

  virtual Roots findRoots(bool censor) override;

  virtual void collectGarbage(const GCOptions& options,
                              GCResults& results) override;

  virtual void optimiseStore() override;

  virtual bool verifyStore(bool checkContents,
                           RepairFlag repair = NoRepair) override;

  virtual void addSignatures(const Path& storePath,
                             const StringSet& sigs) override;

  virtual void queryMissing(const PathSet& targets, PathSet& willBuild,
                            PathSet& willSubstitute, PathSet& unknown,
                            unsigned long long& downloadSize,
                            unsigned long long& narSize) override;

  virtual std::shared_ptr<std::string> getBuildLog(const Path& path) override;

  void connect() override{};

  virtual unsigned int getProtocol() override;

 protected:
  virtual bool isValidPathUncached(const Path& path) override;

  virtual PathSet queryValidPaths(
      const PathSet& paths,
      SubstituteFlag maybeSubstitute = NoSubstitute) override;

  virtual void queryPathInfoUncached(
      const Path& path,
      Callback<std::shared_ptr<ValidPathInfo>> callback) noexcept override;

 private:
  std::optional<std::string> uri_;
  std::unique_ptr<nix::proto::WorkerService::Stub> stub_;

  void const SuccessOrThrow(const grpc::Status& status,
                            const absl::string_view& call = "") const;
};

}  // namespace nix::store
