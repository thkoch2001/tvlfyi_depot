#include "libutil/filesystem.hh"

#include <fstream>

#include <absl/strings/str_cat.h>
#include <absl/strings/str_split.h>
#include <absl/strings/string_view.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "libutil/util.hh"

namespace nix {
namespace util {

absl::StatusCode StatusCodeFromErrno(int errnum) {
  switch (errnum) {
    case ECONNABORTED:
    case ENOLINK:
    case ENOTRECOVERABLE:
    case EOWNERDEAD:
    case EPIPE:
    case ESTALE:
      return absl::StatusCode::kAborted;
    case EEXIST:
      return absl::StatusCode::kAlreadyExists;
    case ECANCELED:
    case ETIME:
      return absl::StatusCode::kCancelled;
    case EBADF:
    case ECHILD:
    case EISCONN:
    case EISDIR:
    case ENOEXEC:
    case ENOTCONN:
    case ENOTEMPTY:
    case ENOTTY:
    case EROFS:
    case ETXTBSY:
    case EXDEV:
      return absl::StatusCode::kFailedPrecondition;
    case EDEADLK:
    case EIO:
    case EPROTO:
      return absl::StatusCode::kInternal;
    case EDESTADDRREQ:
    case EFAULT:
    case EILSEQ:
    case EINVAL:
    case EMULTIHOP:
    case ENOPROTOOPT:
    case ENOSTR:
    case ENOTDIR:
    case ENOTSOCK:
    case EPROTOTYPE:
    case ESPIPE:
      return absl::StatusCode::kInvalidArgument;
    case EIDRM:
    case ENODEV:
    case ENOENT:
    case ENXIO:
    case ESRCH:
      return absl::StatusCode::kNotFound;
    case EDOM:
    case EFBIG:
      return absl::StatusCode::kOutOfRange;
    case EACCES:
    case EPERM:
      return absl::StatusCode::kPermissionDenied;
    case E2BIG:
    case EDQUOT:
    case ELOOP:
    case EMFILE:
    case EMLINK:
    case EMSGSIZE:
    case ENAMETOOLONG:
    case ENFILE:
    case ENOBUFS:
    case ENOLCK:
    case ENOMEM:
    case ENOSPC:
    case ENOSR:
    case EOVERFLOW:
    case ERANGE:
      return absl::StatusCode::kResourceExhausted;
    case EAGAIN:
    case EBUSY:
    case ECONNREFUSED:
    case ECONNRESET:
    case EHOSTUNREACH:
    case EINPROGRESS:
    case EINTR:
    case ENETDOWN:
    case ENETRESET:
    case ENETUNREACH:
    case ENODATA:
    case ETIMEDOUT:
#if EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK:
#endif
      return absl::StatusCode::kUnavailable;
    case ENOSYS:
    case ENOTSUP:
#if EOPNOTSUPP != ENOTSUP
    case EOPNOTSUPP:
#endif
    case EPROTONOSUPPORT:
      return absl::StatusCode::kUnimplemented;
    default:
      return absl::StatusCode::kUnknown;
  }
}

// Support code for StatusFromErrnoFD to get a string description for a file
// descriptor.
static std::string GetFDDescription(int fd) {
  constexpr int kLinkSize = 512;

  std::string procFDPath = absl::StrCat("/proc/self/fd/", fd);
  std::vector<char> linkContent(kLinkSize + 1);
  ssize_t linkRd = readlink(procFDPath.c_str(), linkContent.data(), kLinkSize);
  if (linkRd == -1 || linkRd == 0) {
    return absl::StrCat("<file descriptor ", fd, ">");
  }
  if (linkRd == kLinkSize) {
    linkContent[kLinkSize] = '\0';
    return absl::StrCat(absl::string_view(linkContent.data(), kLinkSize),
                        "...");
  }
  linkContent[linkRd] = '\0';
  return std::string(linkContent.data(), linkRd);
}

absl::Status StatusFromErrno(int errnum, absl::string_view operation) {
  return absl::Status(StatusCodeFromErrno(errnum),
                      absl::StrCat(operation, " ", strerror(errnum)));
}

absl::Status StatusFromErrno(int errnum, absl::string_view operation,
                             absl::string_view subject) {
  return absl::Status(
      StatusCodeFromErrno(errnum),
      absl::StrCat(operation, " ", subject, " ", strerror(errnum)));
}

absl::Status StatusFromErrnoFD(int errnum, absl::string_view operation,
                               int fd) {
  std::string subject = GetFDDescription(fd);
  return StatusFromErrno(errnum, operation, subject);
}

absl::StatusOr<std::string> readFile_Status(int fd) {
  struct stat st;
  if (fstat(fd, &st) == -1) {
    return StatusFromErrnoFD(errno, "stat", fd);
  }

  std::vector<char> buf(st.st_size);
  size_t count = st.st_size;
  size_t off = 0;
  while (count != 0) {
    RETURN_IF_ERROR(checkInterrupt_Status());
    ssize_t res = read(fd, buf.data() + off, count);
    if (res <= -1) {
      if (errno == EINTR) {
        continue;
      }
      return StatusFromErrnoFD(errno, "read", fd);
    }
    if (res == 0) {
      return absl::OutOfRangeError(
          absl::StrCat("unexpected EOF after reading ", st.st_size - count, "/",
                       st.st_size, " bytes from ", GetFDDescription(fd)));
    }
    count -= res;
    off += res;
  }
  return std::string(buf.data(), buf.size());
}

absl::StatusOr<std::string> readFile_Status(absl::string_view path) {
  std::ifstream in(path, std::ios::in | std::ios::binary);
  if (!in) {
    return StatusFromErrno(errno, "readFile: open", path);
  }
  in.exceptions(0);
  errno = 0;

  in.seekg(0, std::ios::end);
  if (!in || errno != 0) {
    return StatusFromErrno(errno, "readFile: seek end", path);
  }

  std::string contents;
  ssize_t size = in.tellg();
  if (!in || errno != 0) {
    return StatusFromErrno(errno, "readFile: seek tell", path);
  }
  contents.resize(size);
  in.seekg(0, std::ios::beg);
  if (!in || errno != 0) {
    return StatusFromErrno(errno, "readFile: seek start", path);
  }

  in.read(&contents[0], contents.size());
  in.close();
  if (!in || errno != 0) {
    return StatusFromErrno(errno, "readFile: read", path);
  }

  return contents;
}

}  // namespace util
}  // namespace nix
