#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"
#include "libstore/store-api.hh"

namespace nix::daemon {

using ::grpc::Status;
using ::nix::proto::StorePath;
using ::nix::proto::StorePaths;
using ::nix::proto::Worker;

class WorkerServiceImpl final : public Worker::Service {
 public:
  WorkerServiceImpl(nix::Store* store) : store_(store) {}

  Status IsValidPath(grpc::ServerContext* context, const StorePath* request,
                     nix::proto::IsValidPathResponse* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);
    response->set_is_valid(store_->isValidPath(path));

    return Status::OK;
  }

  Status HasSubstitutes(grpc::ServerContext* context, const StorePath* request,
                        nix::proto::HasSubstitutesResponse* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);
    PathSet res = store_->querySubstitutablePaths({path});
    response->set_has_substitutes(res.find(path) != res.end());

    return Status::OK;
  }

  Status QueryReferrers(grpc::ServerContext* context, const StorePath* request,
                        StorePaths* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);

    PathSet paths;
    store_->queryReferrers(path, paths);

    for (const auto& path : paths) {
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status QueryValidDerivers(grpc::ServerContext* context,
                            const StorePath* request, StorePaths* response) {
    const auto& path = request->path();
    store_->assertStorePath(path);

    PathSet paths = store_->queryValidDerivers(path);

    for (const auto& path : paths) {
      response->add_paths(path);
    }

    return Status::OK;
  }

  Status QueryDerivationOutputs(grpc::ServerContext* context,
                                const StorePath* request,
                                StorePaths* response) override {
    const auto& path = request->path();
    store_->assertStorePath(path);

    PathSet paths = store_->queryDerivationOutputs(path);

    for (const auto& path : paths) {
      response->add_paths(path);
    }

    return Status::OK;
  }

 private:
  // TODO(tazjin): Who owns the store?
  nix::Store* store_;
};

}  // namespace nix::daemon
