#include "libproto/worker.grpc.pb.h"
#include "libproto/worker.pb.h"
#include "libstore/store-api.hh"

namespace nix::daemon {

using ::grpc::Status;
using ::nix::proto::StorePath;
using ::nix::proto::Worker;

class WorkerServiceImpl final : public Worker::Service {
 public:
  WorkerServiceImpl(nix::Store* store) : store_(store) {}

  Status IsValidPath(grpc::ServerContext* context,
                     const ::nix::proto::StorePath* request,
                     nix::proto::IsValidPathResponse* response) {
    const auto& path = request->path();
    store_->assertStorePath(path);
    response->set_is_valid(store_->isValidPath(path));

    return Status::OK;
  }

 private:
  // TODO(tazjin): Who owns the store?
  nix::Store* store_;
};

}  // namespace nix::daemon
