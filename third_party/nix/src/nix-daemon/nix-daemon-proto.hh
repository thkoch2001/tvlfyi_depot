#pragma once

#include <memory>

#include "libproto/worker.grpc.pb.h"
#include "libstore/store-api.hh"

namespace nix::daemon {

nix::proto::WorkerService::Service* NewWorkerService(
    std::shared_ptr<nix::Store>);

}  // namespace nix::daemon
