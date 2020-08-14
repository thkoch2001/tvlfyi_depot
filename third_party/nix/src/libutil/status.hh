#pragma once

#include <absl/status/status.h>
#include <absl/strings/str_format.h>
#include <absl/strings/string_view.h>

#include "libutil/types.hh"

namespace nix::util {

constexpr absl::string_view AbslStatusCodeDescription(absl::StatusCode code) {
  switch (code) {
    case absl::StatusCode::kOk:
      return "OK";
    case absl::StatusCode::kCancelled:
      return "CANCELLED";
    case absl::StatusCode::kUnknown:
      return "UNKNOWN";
    case absl::StatusCode::kInvalidArgument:
      return "INVALID_ARGUMENT";
    case absl::StatusCode::kDeadlineExceeded:
      return "DEADLINE_EXCEEDED";
    case absl::StatusCode::kNotFound:
      return "NOT_FOUND";
    case absl::StatusCode::kAlreadyExists:
      return "ALREADY_EXISTS";
    case absl::StatusCode::kPermissionDenied:
      return "PERMISSION_DENIED";
    case absl::StatusCode::kUnauthenticated:
      return "UNAUTHENTICATED";
    case absl::StatusCode::kResourceExhausted:
      return "RESOURCE_EXHAUSTED";
    case absl::StatusCode::kFailedPrecondition:
      return "FAILED_PRECONDITION";
    case absl::StatusCode::kAborted:
      return "ABORTED";
    case absl::StatusCode::kOutOfRange:
      return "OUT_OF_RANGE";
    case absl::StatusCode::kUnimplemented:
      return "UNIMPLEMENTED";
    case absl::StatusCode::kInternal:
      return "INTERNAL";
    case absl::StatusCode::kUnavailable:
      return "UNAVAILABLE";
    case absl::StatusCode::kDataLoss:
      return "DATA_LOSS";
    default:
      return "<BAD ERROR CODE>";
  };
}

inline void OkOrThrow(absl::Status status) {
  if (!status.ok()) {
    throw Error(absl::StrFormat("Operation failed (%s): %s",
                                AbslStatusCodeDescription(status.code()),
                                status.message()));
  }
}

}  // namespace nix::util
