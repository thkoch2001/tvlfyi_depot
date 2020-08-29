#pragma once

#include <memory>

#include "libproto/worker.grpc.pb.h"
#include "libstore/store-api.hh"

namespace nix::daemon {

constexpr absl::string_view kCancelledRequestMetadata = "cancelled";
constexpr absl::string_view kNixOptionsIdentifier = "nix.options";

nix::proto::WorkerService::Service* NewWorkerService(nix::Store&);

}  // namespace nix::daemon
