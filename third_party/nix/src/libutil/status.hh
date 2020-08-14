#pragma once

#include <absl/status/status.h>
#include <absl/strings/str_format.h>
#include <absl/strings/string_view.h>

#include "libutil/types.hh"

namespace nix::util {

inline void OkOrThrow(absl::Status status) {
  if (!status.ok()) {
    throw Error(absl::StrFormat("Operation failed (%s): %s",
                                absl::StatusCodeToString(status.code()),
                                status.ToString()));
  }
}

}  // namespace nix::util
