#pragma once

#include <absl/status/status.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/impl/codegen/status_code_enum.h>

#include "libproto/worker.pb.h"
#include "libutil/types.hh"

namespace nix::util::proto {

inline ::nix::proto::StorePath StorePath(const Path& path) {
  ::nix::proto::StorePath store_path;
  store_path.set_path(path);
  return store_path;
}

inline ::nix::proto::StorePaths StorePaths(const PathSet& paths) {
  ::nix::proto::StorePaths result;
  for (const auto& path : paths) {
    result.add_paths(path);
  }
  return result;
}

template <typename T, typename U>
T FillFrom(const U& src) {
  T result;
  result.insert(src.begin(), src.end());
  return result;
}

constexpr absl::StatusCode GRPCStatusCodeToAbsl(grpc::StatusCode code) {
  switch (code) {
    case grpc::StatusCode::OK:
      return absl::StatusCode::kOk;
    case grpc::StatusCode::CANCELLED:
      return absl::StatusCode::kCancelled;
    case grpc::StatusCode::UNKNOWN:
      return absl::StatusCode::kUnknown;
    case grpc::StatusCode::INVALID_ARGUMENT:
      return absl::StatusCode::kInvalidArgument;
    case grpc::StatusCode::DEADLINE_EXCEEDED:
      return absl::StatusCode::kDeadlineExceeded;
    case grpc::StatusCode::NOT_FOUND:
      return absl::StatusCode::kNotFound;
    case grpc::StatusCode::ALREADY_EXISTS:
      return absl::StatusCode::kAlreadyExists;
    case grpc::StatusCode::PERMISSION_DENIED:
      return absl::StatusCode::kPermissionDenied;
    case grpc::StatusCode::UNAUTHENTICATED:
      return absl::StatusCode::kUnauthenticated;
    case grpc::StatusCode::RESOURCE_EXHAUSTED:
      return absl::StatusCode::kResourceExhausted;
    case grpc::StatusCode::FAILED_PRECONDITION:
      return absl::StatusCode::kFailedPrecondition;
    case grpc::StatusCode::ABORTED:
      return absl::StatusCode::kAborted;
    case grpc::StatusCode::OUT_OF_RANGE:
      return absl::StatusCode::kOutOfRange;
    case grpc::StatusCode::UNIMPLEMENTED:
      return absl::StatusCode::kUnimplemented;
    case grpc::StatusCode::INTERNAL:
      return absl::StatusCode::kInternal;
    case grpc::StatusCode::UNAVAILABLE:
      return absl::StatusCode::kUnavailable;
    case grpc::StatusCode::DATA_LOSS:
      return absl::StatusCode::kDataLoss;
    default:
      return absl::StatusCode::kInternal;
  }
}

constexpr grpc::StatusCode AbslStatusCodeToGRPC(absl::StatusCode code) {
  switch (code) {
    case absl::StatusCode::kOk:
      return grpc::StatusCode::OK;
    case absl::StatusCode::kCancelled:
      return grpc::StatusCode::CANCELLED;
    case absl::StatusCode::kUnknown:
      return grpc::StatusCode::UNKNOWN;
    case absl::StatusCode::kInvalidArgument:
      return grpc::StatusCode::INVALID_ARGUMENT;
    case absl::StatusCode::kDeadlineExceeded:
      return grpc::StatusCode::DEADLINE_EXCEEDED;
    case absl::StatusCode::kNotFound:
      return grpc::StatusCode::NOT_FOUND;
    case absl::StatusCode::kAlreadyExists:
      return grpc::StatusCode::ALREADY_EXISTS;
    case absl::StatusCode::kPermissionDenied:
      return grpc::StatusCode::PERMISSION_DENIED;
    case absl::StatusCode::kUnauthenticated:
      return grpc::StatusCode::UNAUTHENTICATED;
    case absl::StatusCode::kResourceExhausted:
      return grpc::StatusCode::RESOURCE_EXHAUSTED;
    case absl::StatusCode::kFailedPrecondition:
      return grpc::StatusCode::FAILED_PRECONDITION;
    case absl::StatusCode::kAborted:
      return grpc::StatusCode::ABORTED;
    case absl::StatusCode::kOutOfRange:
      return grpc::StatusCode::OUT_OF_RANGE;
    case absl::StatusCode::kUnimplemented:
      return grpc::StatusCode::UNIMPLEMENTED;
    case absl::StatusCode::kInternal:
      return grpc::StatusCode::INTERNAL;
    case absl::StatusCode::kUnavailable:
      return grpc::StatusCode::UNAVAILABLE;
    case absl::StatusCode::kDataLoss:
      return grpc::StatusCode::DATA_LOSS;
    default:
      return grpc::StatusCode::INTERNAL;
  }
}

constexpr absl::string_view GRPCStatusCodeDescription(grpc::StatusCode code) {
  switch (code) {
    case grpc::StatusCode::OK:
      return "OK";
    case grpc::StatusCode::CANCELLED:
      return "CANCELLED";
    case grpc::StatusCode::UNKNOWN:
      return "UNKNOWN";
    case grpc::StatusCode::INVALID_ARGUMENT:
      return "INVALID_ARGUMENT";
    case grpc::StatusCode::DEADLINE_EXCEEDED:
      return "DEADLINE_EXCEEDED";
    case grpc::StatusCode::NOT_FOUND:
      return "NOT_FOUND";
    case grpc::StatusCode::ALREADY_EXISTS:
      return "ALREADY_EXISTS";
    case grpc::StatusCode::PERMISSION_DENIED:
      return "PERMISSION_DENIED";
    case grpc::StatusCode::UNAUTHENTICATED:
      return "UNAUTHENTICATED";
    case grpc::StatusCode::RESOURCE_EXHAUSTED:
      return "RESOURCE_EXHAUSTED";
    case grpc::StatusCode::FAILED_PRECONDITION:
      return "FAILED_PRECONDITION";
    case grpc::StatusCode::ABORTED:
      return "ABORTED";
    case grpc::StatusCode::OUT_OF_RANGE:
      return "OUT_OF_RANGE";
    case grpc::StatusCode::UNIMPLEMENTED:
      return "UNIMPLEMENTED";
    case grpc::StatusCode::INTERNAL:
      return "INTERNAL";
    case grpc::StatusCode::UNAVAILABLE:
      return "UNAVAILABLE";
    case grpc::StatusCode::DATA_LOSS:
      return "DATA_LOSS";
    default:
      return "<BAD ERROR CODE>";
  };
}

inline absl::Status GRPCStatusToAbsl(grpc::Status status) {
  if (status.ok()) {
    return absl::OkStatus();
  }

  return absl::Status(GRPCStatusCodeToAbsl(status.error_code()),
                      status.error_message());
}

inline grpc::Status AbslToGRPCStatus(absl::Status status) {
  if (status.ok()) {
    return grpc::Status::OK;
  }

  return grpc::Status(AbslStatusCodeToGRPC(status.code()),
                      std::string(status.message()));
}

}  // namespace nix::util::proto
