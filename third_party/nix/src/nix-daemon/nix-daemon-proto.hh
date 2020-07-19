#pragma once

#include <memory>

#include "libproto/worker.grpc.pb.h"
#include "libstore/store-api.hh"

namespace nix::daemon {

static std::unique_ptr<nix::proto::WorkerService::Service> NewWorkerService(
    nix::Store*);

}  // namespace nix::daemon
