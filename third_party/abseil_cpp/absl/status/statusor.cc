/* Copyright 2017 The TensorFlow Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
==============================================================================*/

#include "absl/status/statusor.h"

#include "absl/base/internal/raw_logging.h"

namespace absl {
ABSL_NAMESPACE_BEGIN

namespace internal_statusor {

#define ABSL_STATUSOR_INTERNAL_BAD_OK_MSG "An OK status is not a valid " \
  "constructor argument to StatusOr<T>"

void Helper::HandleInvalidStatusCtorArg(Status* status) {
  ABSL_RAW_LOG(ERROR, ABSL_STATUSOR_INTERNAL_BAD_OK_MSG);
  // Fall back to kInternal.
  *status = InternalError(ABSL_STATUSOR_INTERNAL_BAD_OK_MSG);
}

#undef ABSL_STATUSOR_INTERNAL_BAD_OK_MSG

void Helper::Crash(const Status& status) {
#ifdef ABSL_HAVE_EXCEPTIONS
  throw status;
#else
  std::string status_debug = status.ToString();
  ABSL_RAW_LOG(FATAL, "Attempting to fetch value instead of handling error: %s", status_debug.c_str());
  abort();   // TODO(calabrese) Remove once RAW_LOG FATAL is noreturn.
#endif
}
}  // namespace internal_statusor

ABSL_NAMESPACE_END
}  // namespace absl
