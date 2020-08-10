#pragma once

#include <absl/status/statusor.h>
#include <absl/strings/string_view.h>

namespace nix::util {

/* absl::Status functions */

// Deduce a reasonable absl::StatusCode from an errno value.
absl::StatusCode StatusCodeFromErrno(int errnum);

// Construct a status from an errno return.
//
// Format: 'operation subject: error'
absl::Status StatusFromErrno(int errnum, absl::string_view operation,
                             absl::string_view subject);

// Construct a status from an errno return.
//
// Format: 'operation: error'
absl::Status StatusFromErrno(int errnum, absl::string_view operation);

// Construct a status from a file operation errno return.
//
// Format: 'operation /path/to/file: error'
absl::Status StatusFromErrnoFD(int errnum, absl::string_view operation, int fd);

/* File reading / writing */

// Read exactly the number of bytes from the given file descriptor that fstat()
// says are available.
absl::StatusOr<std::string> readFile_Status(int fd);

// Fully read the file at the given path.
absl::StatusOr<std::string> readFile_Status(absl::string_view path);

}  // namespace nix::util
