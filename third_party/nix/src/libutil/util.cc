#include "libutil/util.hh"

#include <cctype>
#include <cerrno>
#include <climits>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <future>
#include <iostream>
#include <sstream>
#include <thread>
#include <utility>

#include <absl/strings/str_split.h>
#include <absl/strings/string_view.h>
#include <fcntl.h>
#include <glog/logging.h>
#include <grp.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "libutil/affinity.hh"
#include "libutil/finally.hh"
#include "libutil/lazy.hh"
#include "libutil/serialise.hh"
#include "libutil/sync.hh"
#include "nix_config.h"

namespace nix {

const std::string nativeSystem = SYSTEM;

BaseError& BaseError::addPrefix(const FormatOrString& fs) {
  prefix_ = fs.s + prefix_;
  return *this;
}

std::string SysError::addErrno(const std::string& s) {
  errNo = errno;
  return s + ": " + strerror(errNo);
}

std::string getEnv(const std::string& key, const std::string& def) {
  char* value = getenv(key.c_str());
  return value != nullptr ? std::string(value) : def;
}

std::map<std::string, std::string> getEnv() {
  std::map<std::string, std::string> env;
  for (size_t i = 0; environ[i] != nullptr; ++i) {
    auto s = environ[i];
    auto eq = strchr(s, '=');
    if (eq == nullptr) {
      // invalid env, just keep going
      continue;
    }
    env.emplace(std::string(s, eq), std::string(eq + 1));
  }
  return env;
}

void clearEnv() {
  for (auto& name : getEnv()) {
    unsetenv(name.first.c_str());
  }
}

void replaceEnv(const std::map<std::string, std::string>& newEnv) {
  clearEnv();
  for (const auto& newEnvVar : newEnv) {
    setenv(newEnvVar.first.c_str(), newEnvVar.second.c_str(), 1);
  }
}

Path absPath(Path path, Path dir) {
  if (path[0] != '/') {
    if (dir.empty()) {
#ifdef __GNU__
      /* GNU (aka. GNU/Hurd) doesn't have any limitation on path
         lengths and doesn't define `PATH_MAX'.  */
      char* buf = getcwd(NULL, 0);
      if (buf == NULL)
#else
      char buf[PATH_MAX];
      if (getcwd(buf, sizeof(buf)) == nullptr) {
#endif
        throw SysError("cannot get cwd");
    }
    dir = buf;
#ifdef __GNU__
    free(buf);
#endif
  }
  path = dir + "/" + path;
}
return canonPath(path);
}  // namespace nix

Path canonPath(const Path& path, bool resolveSymlinks) {
  assert(!path.empty());

  std::string s;

  if (path[0] != '/') {
    throw Error(format("not an absolute path: '%1%'") % path);
  }

  std::string::const_iterator i = path.begin();
  std::string::const_iterator end = path.end();
  std::string temp;

  /* Count the number of times we follow a symlink and stop at some
     arbitrary (but high) limit to prevent infinite loops. */
  unsigned int followCount = 0;
  unsigned int maxFollow = 1024;

  while (true) {
    /* Skip slashes. */
    while (i != end && *i == '/') {
      i++;
    }
    if (i == end) {
      break;
    }

    /* Ignore `.'. */
    if (*i == '.' && (i + 1 == end || i[1] == '/')) {
      i++;
    }

    /* If `..', delete the last component. */
    else if (*i == '.' && i + 1 < end && i[1] == '.' &&
             (i + 2 == end || i[2] == '/')) {
      if (!s.empty()) {
        s.erase(s.rfind('/'));
      }
      i += 2;
    }

    /* Normal component; copy it. */
    else {
      s += '/';
      while (i != end && *i != '/') {
        s += *i++;
      }

      /* If s points to a symlink, resolve it and restart (since
         the symlink target might contain new symlinks). */
      if (resolveSymlinks && isLink(s)) {
        if (++followCount >= maxFollow) {
          throw Error(format("infinite symlink recursion in path '%1%'") %
                      path);
        }
        temp = absPath(readLink(s), dirOf(s)) + std::string(i, end);
        i = temp.begin(); /* restart */
        end = temp.end();
        s = "";
      }
    }
  }

  return s.empty() ? "/" : s;
}

// TODO(grfn) remove in favor of std::filesystem::path::parent_path()
Path dirOf(absl::string_view path) {
  Path::size_type pos = path.rfind('/');
  if (pos == std::string::npos) {
    return ".";
  }
  return pos == 0 ? "/" : Path(path, 0, pos);
}

// TODO(grfn) remove in favor of std::filesystem::path::root_name()
std::string baseNameOf(const Path& path) {
  if (path.empty()) {
    return "";
  }

  Path::size_type last = path.length() - 1;
  if (path[last] == '/' && last > 0) {
    last -= 1;
  }

  Path::size_type pos = path.rfind('/', last);
  if (pos == std::string::npos) {
    pos = 0;
  } else {
    pos += 1;
  }

  return std::string(path, pos, last - pos + 1);
}

bool isInDir(const Path& path, const Path& dir) {
  return path[0] == '/' && std::string(path, 0, dir.size()) == dir &&
         path.size() >= dir.size() + 2 && path[dir.size()] == '/';
}

bool isDirOrInDir(const Path& path, const Path& dir) {
  return path == dir || isInDir(path, dir);
}

struct stat lstat(const Path& path) {
  struct stat st;
  if (lstat(path.c_str(), &st) != 0) {
    throw SysError(format("getting status of '%1%'") % path);
  }
  return st;
}

bool pathExists(const Path& path) {
  int res;
  struct stat st;
  res = lstat(path.c_str(), &st);
  if (res == 0) {
    return true;
  }
  if (errno != ENOENT && errno != ENOTDIR) {
    throw SysError(format("getting status of %1%") % path);
  }
  return false;
}

Path readLink(const Path& path) {
  checkInterrupt();
  std::vector<char> buf;
  for (ssize_t bufSize = PATH_MAX / 4; true; bufSize += bufSize / 2) {
    buf.resize(bufSize);
    ssize_t rlSize = readlink(path.c_str(), buf.data(), bufSize);
    if (rlSize == -1) {
      if (errno == EINVAL) {
        throw Error("'%1%' is not a symlink", path);
      }
      throw SysError("reading symbolic link '%1%'", path);

    } else if (rlSize < bufSize) {
      return std::string(buf.data(), rlSize);
    }
  }
}

bool isLink(const Path& path) {
  struct stat st = lstat(path);
  return S_ISLNK(st.st_mode);
}

DirEntries readDirectory(DIR* dir, const Path& path) {
  DirEntries entries;
  entries.reserve(64);

  struct dirent* dirent;
  while (errno = 0, dirent = readdir(dir)) { /* sic */
    checkInterrupt();
    std::string name = dirent->d_name;
    if (name == "." || name == "..") {
      continue;
    }
    entries.emplace_back(name, dirent->d_ino,
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
                         dirent->d_type
#else
                         DT_UNKNOWN
#endif
    );
  }
  if (errno) {
    throw SysError(format("reading directory '%1%'") % path);
  }

  return entries;
}

DirEntries readDirectory(const Path& path) {
  AutoCloseDir dir(opendir(path.c_str()));
  if (!dir) {
    throw SysError(format("opening directory '%1%'") % path);
  }

  return readDirectory(dir.get(), path);
}

unsigned char getFileType(const Path& path) {
  struct stat st = lstat(path);
  if (S_ISDIR(st.st_mode)) {
    return DT_DIR;
  }
  if (S_ISLNK(st.st_mode)) {
    return DT_LNK;
  }
  if (S_ISREG(st.st_mode)) {
    return DT_REG;
  }
  return DT_UNKNOWN;
}

std::string readFile(int fd) {
  struct stat st;
  if (fstat(fd, &st) == -1) {
    throw SysError("statting file");
  }

  std::vector<unsigned char> buf(st.st_size);
  readFull(fd, buf.data(), st.st_size);

  return std::string((char*)buf.data(), st.st_size);
}

std::string readFile(absl::string_view path, bool drain) {
  AutoCloseFD fd = open(std::string(path).c_str(), O_RDONLY | O_CLOEXEC);
  if (!fd) {
    throw SysError(format("opening file '%1%'") % path);
  }
  return drain ? drainFD(fd.get()) : readFile(fd.get());
}

void readFile(absl::string_view path, Sink& sink) {
  // TODO(tazjin): use stdlib functions for this stuff
  AutoCloseFD fd = open(std::string(path).c_str(), O_RDONLY | O_CLOEXEC);
  if (!fd) {
    throw SysError("opening file '%s'", path);
  }
  drainFD(fd.get(), sink);
}

void writeFile(const Path& path, const std::string& s, mode_t mode) {
  AutoCloseFD fd =
      open(path.c_str(), O_WRONLY | O_TRUNC | O_CREAT | O_CLOEXEC, mode);
  if (!fd) {
    throw SysError(format("opening file '%1%'") % path);
  }
  writeFull(fd.get(), s);
}

void writeFile(const Path& path, Source& source, mode_t mode) {
  AutoCloseFD fd =
      open(path.c_str(), O_WRONLY | O_TRUNC | O_CREAT | O_CLOEXEC, mode);
  if (!fd) {
    throw SysError(format("opening file '%1%'") % path);
  }

  std::vector<unsigned char> buf(64 * 1024);

  while (true) {
    try {
      auto n = source.read(buf.data(), buf.size());
      writeFull(fd.get(), (unsigned char*)buf.data(), n);
    } catch (EndOfFile&) {
      break;
    }
  }
}

std::string readLine(int fd) {
  std::string s;
  while (true) {
    checkInterrupt();
    char ch;
    // FIXME: inefficient
    ssize_t rd = read(fd, &ch, 1);
    if (rd == -1) {
      if (errno != EINTR) {
        throw SysError("reading a line");
      }
    } else if (rd == 0) {
      throw EndOfFile("unexpected EOF reading a line");
    } else {
      if (ch == '\n') {
        return s;
      }
      s += ch;
    }
  }
}

void writeLine(int fd, std::string s) {
  s += '\n';
  writeFull(fd, s);
}

static void _deletePath(int parentfd, const Path& path,
                        unsigned long long& bytesFreed) {
  checkInterrupt();

  std::string name(baseNameOf(path));

  struct stat st;
  if (fstatat(parentfd, name.c_str(), &st, AT_SYMLINK_NOFOLLOW) == -1) {
    if (errno == ENOENT) {
      return;
    }
    throw SysError(format("getting status of '%1%'") % path);
  }

  if (!S_ISDIR(st.st_mode) && st.st_nlink == 1) {
    bytesFreed += st.st_size;
  }

  if (S_ISDIR(st.st_mode)) {
    /* Make the directory accessible. */
    const auto PERM_MASK = S_IRUSR | S_IWUSR | S_IXUSR;
    if ((st.st_mode & PERM_MASK) != PERM_MASK) {
      if (fchmodat(parentfd, name.c_str(), st.st_mode | PERM_MASK, 0) == -1) {
        throw SysError(format("chmod '%1%'") % path);
      }
    }

    int fd = openat(parentfd, path.c_str(), O_RDONLY);
    if (!fd) {
      throw SysError(format("opening directory '%1%'") % path);
    }
    AutoCloseDir dir(fdopendir(fd));
    if (!dir) {
      throw SysError(format("opening directory '%1%'") % path);
    }
    for (auto& i : readDirectory(dir.get(), path)) {
      _deletePath(dirfd(dir.get()), path + "/" + i.name, bytesFreed);
    }
  }

  int flags = S_ISDIR(st.st_mode) ? AT_REMOVEDIR : 0;
  if (unlinkat(parentfd, name.c_str(), flags) == -1) {
    if (errno == ENOENT) {
      return;
    }
    throw SysError(format("cannot unlink '%1%'") % path);
  }
}

static void _deletePath(const Path& path, unsigned long long& bytesFreed) {
  Path dir = dirOf(path);
  if (dir == "") dir = "/";

  AutoCloseFD dirfd(open(dir.c_str(), O_RDONLY));
  if (!dirfd) {
    // This really shouldn't fail silently, but it's left this way
    // for backwards compatibility.
    if (errno == ENOENT) return;

    throw SysError(format("opening directory '%1%'") % path);
  }

  _deletePath(dirfd.get(), path, bytesFreed);
}

void deletePath(const Path& path) {
  unsigned long long dummy;
  deletePath(path, dummy);
}

void deletePath(const Path& path, unsigned long long& bytesFreed) {
  // Activity act(*logger, lvlDebug, format("recursively deleting path '%1%'") %
  // path);
  bytesFreed = 0;
  _deletePath(path, bytesFreed);
}

static Path tempName(Path tmpRoot, const Path& prefix, bool includePid,
                     int& counter) {
  tmpRoot =
      canonPath(tmpRoot.empty() ? getEnv("TMPDIR", "/tmp") : tmpRoot, true);
  if (includePid) {
    return (format("%1%/%2%-%3%-%4%") % tmpRoot % prefix % getpid() % counter++)
        .str();
  }
  return (format("%1%/%2%-%3%") % tmpRoot % prefix % counter++).str();
}

Path createTempDir(const Path& tmpRoot, const Path& prefix, bool includePid,
                   bool useGlobalCounter, mode_t mode) {
  static int globalCounter = 0;
  int localCounter = 0;
  int& counter(useGlobalCounter ? globalCounter : localCounter);

  while (true) {
    checkInterrupt();
    Path tmpDir = tempName(tmpRoot, prefix, includePid, counter);
    if (mkdir(tmpDir.c_str(), mode) == 0) {
#if __FreeBSD__
      /* Explicitly set the group of the directory.  This is to
         work around around problems caused by BSD's group
         ownership semantics (directories inherit the group of
         the parent).  For instance, the group of /tmp on
         FreeBSD is "wheel", so all directories created in /tmp
         will be owned by "wheel"; but if the user is not in
         "wheel", then "tar" will fail to unpack archives that
         have the setgid bit set on directories. */
      if (chown(tmpDir.c_str(), (uid_t)-1, getegid()) != 0)
        throw SysError(format("setting group of directory '%1%'") % tmpDir);
#endif
      return tmpDir;
    }
    if (errno != EEXIST) {
      throw SysError(format("creating directory '%1%'") % tmpDir);
    }
  }
}

std::string getUserName() {
  auto pw = getpwuid(geteuid());
  std::string name = pw != nullptr ? pw->pw_name : getEnv("USER", "");
  if (name.empty()) {
    throw Error("cannot figure out user name");
  }
  return name;
}

static Lazy<Path> getHome2([]() {
  Path homeDir = getEnv("HOME");
  if (homeDir.empty()) {
    std::vector<char> buf(16384);
    struct passwd pwbuf;
    struct passwd* pw;
    if (getpwuid_r(geteuid(), &pwbuf, buf.data(), buf.size(), &pw) != 0 ||
        (pw == nullptr) || (pw->pw_dir == nullptr) || (pw->pw_dir[0] == 0)) {
      throw Error("cannot determine user's home directory");
    }
    homeDir = pw->pw_dir;
  }
  return homeDir;
});

Path getHome() { return getHome2(); }

Path getCacheDir() {
  Path cacheDir = getEnv("XDG_CACHE_HOME");
  if (cacheDir.empty()) {
    cacheDir = getHome() + "/.cache";
  }
  return cacheDir;
}

Path getConfigDir() {
  Path configDir = getEnv("XDG_CONFIG_HOME");
  if (configDir.empty()) {
    configDir = getHome() + "/.config";
  }
  return configDir;
}

std::vector<Path> getConfigDirs() {
  Path configHome = getConfigDir();
  std::string configDirs = getEnv("XDG_CONFIG_DIRS");
  std::vector<std::string> result =
      absl::StrSplit(configDirs, absl::ByChar(':'));
  result.insert(result.begin(), configHome);
  return result;
}

Path getDataDir() {
  Path dataDir = getEnv("XDG_DATA_HOME");
  if (dataDir.empty()) {
    dataDir = getHome() + "/.local/share";
  }
  return dataDir;
}

// TODO(grfn): Remove in favor of std::filesystem::create_directories
Paths createDirs(const Path& path) {
  Paths created;
  if (path == "/") {
    return created;
  }

  struct stat st;
  if (lstat(path.c_str(), &st) == -1) {
    created = createDirs(dirOf(path));
    if (mkdir(path.c_str(), 0777) == -1 && errno != EEXIST) {
      throw SysError(format("creating directory '%1%'") % path);
    }
    st = lstat(path);
    created.push_back(path);
  }

  if (S_ISLNK(st.st_mode) && stat(path.c_str(), &st) == -1) {
    throw SysError(format("statting symlink '%1%'") % path);
  }

  if (!S_ISDIR(st.st_mode)) {
    throw Error(format("'%1%' is not a directory") % path);
  }

  return created;
}

void createSymlink(const Path& target, const Path& link) {
  if (symlink(target.c_str(), link.c_str()) != 0) {
    throw SysError(format("creating symlink from '%1%' to '%2%'") % link %
                   target);
  }
}

void replaceSymlink(const Path& target, const Path& link) {
  for (unsigned int n = 0; true; n++) {
    Path tmp = canonPath(fmt("%s/.%d_%s", dirOf(link), n, baseNameOf(link)));

    try {
      createSymlink(target, tmp);
    } catch (SysError& e) {
      if (e.errNo == EEXIST) {
        continue;
      }
      throw;
    }

    if (rename(tmp.c_str(), link.c_str()) != 0) {
      throw SysError(format("renaming '%1%' to '%2%'") % tmp % link);
    }

    break;
  }
}

void readFull(int fd, unsigned char* buf, size_t count) {
  while (count != 0u) {
    checkInterrupt();
    ssize_t res = read(fd, (char*)buf, count);
    if (res == -1) {
      if (errno == EINTR) {
        continue;
      }
      throw SysError("reading from file");
    }
    if (res == 0) {
      throw EndOfFile("unexpected end-of-file");
    }
    count -= res;
    buf += res;
  }
}

void writeFull(int fd, const unsigned char* buf, size_t count,
               bool allowInterrupts) {
  while (count != 0u) {
    if (allowInterrupts) {
      checkInterrupt();
    }
    ssize_t res = write(fd, (char*)buf, count);
    if (res == -1 && errno != EINTR) {
      throw SysError("writing to file");
    }
    if (res > 0) {
      count -= res;
      buf += res;
    }
  }
}

void writeFull(int fd, const std::string& s, bool allowInterrupts) {
  writeFull(fd, (const unsigned char*)s.data(), s.size(), allowInterrupts);
}

std::string drainFD(int fd, bool block) {
  StringSink sink;
  drainFD(fd, sink, block);
  return std::move(*sink.s);
}

void drainFD(int fd, Sink& sink, bool block) {
  int saved;

  Finally finally([&]() {
    if (!block) {
      if (fcntl(fd, F_SETFL, saved) == -1) {
        throw SysError("making file descriptor blocking");
      }
    }
  });

  if (!block) {
    saved = fcntl(fd, F_GETFL);
    if (fcntl(fd, F_SETFL, saved | O_NONBLOCK) == -1) {
      throw SysError("making file descriptor non-blocking");
    }
  }

  std::vector<unsigned char> buf(64 * 1024);
  while (true) {
    checkInterrupt();
    ssize_t rd = read(fd, buf.data(), buf.size());
    if (rd == -1) {
      if (!block && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        break;
      }
      if (errno != EINTR) {
        throw SysError("reading from file");
      }
    } else if (rd == 0) {
      break;
    } else {
      sink(buf.data(), rd);
    }
  }
}

//////////////////////////////////////////////////////////////////////

AutoDelete::AutoDelete() : del{false} {}

AutoDelete::AutoDelete(std::string p, bool recursive) : path(std::move(p)) {
  del = true;
  this->recursive = recursive;
}

AutoDelete::~AutoDelete() {
  try {
    if (del) {
      if (recursive) {
        deletePath(path);
      } else {
        if (remove(path.c_str()) == -1) {
          throw SysError(format("cannot unlink '%1%'") % path);
        }
      }
    }
  } catch (...) {
    ignoreException();
  }
}

void AutoDelete::cancel() { del = false; }

void AutoDelete::reset(const Path& p, bool recursive) {
  path = p;
  this->recursive = recursive;
  del = true;
}

//////////////////////////////////////////////////////////////////////

AutoCloseFD::AutoCloseFD() : fd{-1} {}

AutoCloseFD::AutoCloseFD(int fd) : fd{fd} {}

AutoCloseFD::AutoCloseFD(AutoCloseFD&& that) : fd{that.fd} { that.fd = -1; }

AutoCloseFD& AutoCloseFD::operator=(AutoCloseFD&& that) {
  close();
  fd = that.fd;
  that.fd = -1;
  return *this;
}

AutoCloseFD::~AutoCloseFD() {
  try {
    close();
  } catch (...) {
    ignoreException();
  }
}

int AutoCloseFD::get() const { return fd; }

void AutoCloseFD::close() {
  if (fd != -1) {
    if (::close(fd) == -1) { /* This should never happen. */
      throw SysError(format("closing file descriptor %1%") % fd);
    }
  }
}

AutoCloseFD::operator bool() const { return fd != -1; }

int AutoCloseFD::release() {
  int oldFD = fd;
  fd = -1;
  return oldFD;
}

void Pipe::create() {
  int fds[2];
#if HAVE_PIPE2
  if (pipe2(fds, O_CLOEXEC) != 0) {
    throw SysError("creating pipe");
  }
#else
  if (pipe(fds) != 0) {
    throw SysError("creating pipe");
  }
  closeOnExec(fds[0]);
  closeOnExec(fds[1]);
#endif
  readSide = fds[0];
  writeSide = fds[1];
}

//////////////////////////////////////////////////////////////////////

Pid::Pid() = default;

Pid::Pid(pid_t pid) : pid(pid) {}

Pid::~Pid() {
  if (pid != -1) {
    kill();
  }
}

void Pid::operator=(pid_t pid) {
  if (this->pid != -1 && this->pid != pid) {
    kill();
  }
  this->pid = pid;
  killSignal = SIGKILL;  // reset signal to default
}

Pid::operator pid_t() { return pid; }

int Pid::kill() {
  assert(pid != -1);

  DLOG(INFO) << "killing process " << pid;

  /* Send the requested signal to the child.  If it has its own
     process group, send the signal to every process in the child
     process group (which hopefully includes *all* its children). */
  if (::kill(separatePG ? -pid : pid, killSignal) != 0) {
    LOG(ERROR) << SysError("killing process %d", pid).msg();
  }

  return wait();
}

int Pid::wait() {
  assert(pid != -1);
  while (true) {
    int status;
    int res = waitpid(pid, &status, 0);
    if (res == pid) {
      pid = -1;
      return status;
    }
    if (errno != EINTR) {
      throw SysError("cannot get child exit status");
    }
    checkInterrupt();
  }
}

void Pid::setSeparatePG(bool separatePG) { this->separatePG = separatePG; }

void Pid::setKillSignal(int signal) { this->killSignal = signal; }

pid_t Pid::release() {
  pid_t p = pid;
  pid = -1;
  return p;
}

void killUser(uid_t uid) {
  DLOG(INFO) << "killing all processes running under UID " << uid;

  assert(uid != 0); /* just to be safe... */

  /* The system call kill(-1, sig) sends the signal `sig' to all
     users to which the current process can send signals.  So we
     fork a process, switch to uid, and send a mass kill. */

  ProcessOptions options;

  Pid pid = startProcess(
      [&]() {
        if (setuid(uid) == -1) {
          throw SysError("setting uid");
        }

        while (true) {
          if (kill(-1, SIGKILL) == 0) {
            break;
          }
          if (errno == ESRCH) {
            break;
          } /* no more processes */
          if (errno != EINTR) {
            throw SysError(format("cannot kill processes for uid '%1%'") % uid);
          }
        }

        _exit(0);
      },
      options);

  int status = pid.wait();
  if (status != 0) {
    throw Error(format("cannot kill processes for uid '%1%': %2%") % uid %
                statusToString(status));
  }

  /* !!! We should really do some check to make sure that there are
     no processes left running under `uid', but there is no portable
     way to do so (I think).  The most reliable way may be `ps -eo
     uid | grep -q $uid'. */
}

//////////////////////////////////////////////////////////////////////

/*
 * Please note that it is not legal for this function to call vfork().  If the
 * process created by vfork() returns from the function in which vfork() was
 * called, or calls any other function before successfully calling _exit() or
 * one of the exec*() family of functions, the behavior is undefined.
 */
static pid_t doFork(const std::function<void()>& fun) __attribute__((noinline));
static pid_t doFork(const std::function<void()>& fun) {
#ifdef __linux__
  // TODO(kanepyork): call clone() instead for faster forking
#endif

  pid_t pid = fork();
  if (pid != 0) {
    return pid;
  }
  fun();
  abort();
}

pid_t startProcess(std::function<void()> fun, const ProcessOptions& options) {
  auto wrapper = [&]() {
    try {
#if __linux__
      if (options.dieWithParent && prctl(PR_SET_PDEATHSIG, SIGKILL) == -1) {
        throw SysError("setting death signal");
      }
#endif
      restoreAffinity();
      fun();
    } catch (std::exception& e) {
      try {
        LOG(ERROR) << options.errorPrefix << e.what();
      } catch (...) {
      }
    } catch (...) {
    }
    if (options.runExitHandlers) {
      exit(1);
    } else {
      _exit(1);
    }
  };

  pid_t pid = doFork(wrapper);
  if (pid == -1) {
    throw SysError("unable to fork");
  }

  return pid;
}

std::vector<char*> stringsToCharPtrs(const Strings& ss) {
  std::vector<char*> res;
  for (auto& s : ss) {
    res.push_back((char*)s.c_str());
  }
  res.push_back(nullptr);
  return res;
}

std::string runProgram(const Path& program, bool searchPath,
                       const Strings& args,
                       const std::optional<std::string>& input) {
  RunOptions opts(program, args);
  opts.searchPath = searchPath;
  opts.input = input;

  auto res = runProgram(opts);

  if (!statusOk(res.first)) {
    throw ExecError(res.first, fmt("program '%1%' %2%", program,
                                   statusToString(res.first)));
  }

  return res.second;
}

std::pair<int, std::string> runProgram(const RunOptions& options_) {
  RunOptions options(options_);
  StringSink sink;
  options.standardOut = &sink;

  int status = 0;

  try {
    runProgram2(options);
  } catch (ExecError& e) {
    status = e.status;
  }

  return {status, std::move(*sink.s)};
}

void runProgram2(const RunOptions& options) {
  checkInterrupt();

  assert(!(options.standardIn && options.input));

  std::unique_ptr<Source> source_;
  Source* source = options.standardIn;

  if (options.input) {
    source_ = std::make_unique<StringSource>(*options.input);
    source = source_.get();
  }

  /* Create a pipe. */
  Pipe out;
  Pipe in;
  if (options.standardOut != nullptr) {
    out.create();
  }
  if (source != nullptr) {
    in.create();
  }

  ProcessOptions processOptions;

  /* Fork. */
  Pid pid = startProcess(
      [&]() {
        if (options.environment) {
          replaceEnv(*options.environment);
        }
        if ((options.standardOut != nullptr) &&
            dup2(out.writeSide.get(), STDOUT_FILENO) == -1) {
          throw SysError("dupping stdout");
        }
        if (options.mergeStderrToStdout) {
          if (dup2(STDOUT_FILENO, STDERR_FILENO) == -1) {
            throw SysError("cannot dup stdout into stderr");
          }
        }
        if ((source != nullptr) &&
            dup2(in.readSide.get(), STDIN_FILENO) == -1) {
          throw SysError("dupping stdin");
        }

        if (options.chdir && chdir((*options.chdir).c_str()) == -1) {
          throw SysError("chdir failed");
        }
        if (options.gid && setgid(*options.gid) == -1) {
          throw SysError("setgid failed");
        }
        /* Drop all other groups if we're setgid. */
        if (options.gid && setgroups(0, nullptr) == -1) {
          throw SysError("setgroups failed");
        }
        if (options.uid && setuid(*options.uid) == -1) {
          throw SysError("setuid failed");
        }

        Strings args_(options.args);
        args_.push_front(options.program);

        restoreSignals();

        if (options.searchPath) {
          execvp(options.program.c_str(), stringsToCharPtrs(args_).data());
        } else {
          execv(options.program.c_str(), stringsToCharPtrs(args_).data());
        }

        throw SysError("executing '%1%'", options.program);
      },
      processOptions);

  out.writeSide = -1;

  std::thread writerThread;

  std::promise<void> promise;

  Finally doJoin([&]() {
    if (writerThread.joinable()) {
      writerThread.join();
    }
  });

  if (source != nullptr) {
    in.readSide = -1;
    writerThread = std::thread([&]() {
      try {
        std::vector<unsigned char> buf(8 * 1024);
        while (true) {
          size_t n;
          try {
            n = source->read(buf.data(), buf.size());
          } catch (EndOfFile&) {
            break;
          }
          writeFull(in.writeSide.get(), buf.data(), n);
        }
        promise.set_value();
      } catch (...) {
        promise.set_exception(std::current_exception());
      }
      in.writeSide = -1;
    });
  }

  if (options.standardOut != nullptr) {
    drainFD(out.readSide.get(), *options.standardOut);
  }

  /* Wait for the child to finish. */
  int status = pid.wait();

  /* Wait for the writer thread to finish. */
  if (source != nullptr) {
    promise.get_future().get();
  }

  if (status != 0) {
    throw ExecError(status, fmt("program '%1%' %2%", options.program,
                                statusToString(status)));
  }
}

void closeMostFDs(const std::set<int>& exceptions) {
#if __linux__
  try {
    for (auto& s : readDirectory("/proc/self/fd")) {
      auto fd = std::stoi(s.name);
      if (exceptions.count(fd) == 0u) {
        DLOG(INFO) << "closing leaked FD " << fd;
        close(fd);
      }
    }
    return;
  } catch (SysError&) {
  }
#endif

  int maxFD = 0;
  maxFD = sysconf(_SC_OPEN_MAX);
  for (int fd = 0; fd < maxFD; ++fd) {
    if (exceptions.count(fd) == 0u) {
      close(fd);
    } /* ignore result */
  }
}

void closeOnExec(int fd) {
  int prev;
  if ((prev = fcntl(fd, F_GETFD, 0)) == -1 ||
      fcntl(fd, F_SETFD, prev | FD_CLOEXEC) == -1) {
    throw SysError("setting close-on-exec flag");
  }
}

//////////////////////////////////////////////////////////////////////

bool _isInterrupted = false;

static thread_local bool interruptThrown = false;
thread_local std::function<bool()> interruptCheck;

void setInterruptThrown() { interruptThrown = true; }

void _interrupted() {
  /* Block user interrupts while an exception is being handled.
     Throwing an exception while another exception is being handled
     kills the program! */
  if (!interruptThrown && (std::uncaught_exceptions() == 0)) {
    interruptThrown = true;
    throw Interrupted("interrupted by the user");
  }
}

//////////////////////////////////////////////////////////////////////

std::string concatStringsSep(const std::string& sep, const Strings& ss) {
  std::string s;
  for (auto& i : ss) {
    if (!s.empty()) {
      s += sep;
    }
    s += i;
  }
  return s;
}

std::string concatStringsSep(const std::string& sep, const StringSet& ss) {
  std::string s;
  for (auto& i : ss) {
    if (!s.empty()) {
      s += sep;
    }
    s += i;
  }
  return s;
}

std::string replaceStrings(const std::string& s, const std::string& from,
                           const std::string& to) {
  if (from.empty()) {
    return s;
  }
  std::string res = s;
  size_t pos = 0;
  while ((pos = res.find(from, pos)) != std::string::npos) {
    res.replace(pos, from.size(), to);
    pos += to.size();
  }
  return res;
}

std::string statusToString(int status) {
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    if (WIFEXITED(status)) {
      return (format("failed with exit code %1%") % WEXITSTATUS(status)).str();
    }
    if (WIFSIGNALED(status)) {
      int sig = WTERMSIG(status);
#if HAVE_STRSIGNAL
      const char* description = strsignal(sig);
      return (format("failed due to signal %1% (%2%)") % sig % description)
          .str();
#else
      return (format("failed due to signal %1%") % sig).str();
#endif
    } else {
      return "died abnormally";
    }
  } else {
    return "succeeded";
  }
}

bool statusOk(int status) {
  return WIFEXITED(status) && WEXITSTATUS(status) == 0;
}

std::string toLower(const std::string& s) {
  std::string r(s);
  for (auto& c : r) {
    c = std::tolower(c);
  }
  return r;
}

std::string shellEscape(const std::string& s) {
  std::string r = "'";
  for (auto& i : s) {
    if (i == '\'') {
      r += "'\\''";
    } else {
      r += i;
    }
  }
  r += '\'';
  return r;
}

void ignoreException() {
  try {
    throw;
  } catch (std::exception& e) {
    LOG(ERROR) << "error (ignored): " << e.what();
  }
}

std::string filterANSIEscapes(const std::string& s, bool filterAll,
                              unsigned int width) {
  std::string t;
  std::string e;
  size_t w = 0;
  auto i = s.begin();

  while (w < (size_t)width && i != s.end()) {
    if (*i == '\e') {
      std::string e;
      e += *i++;
      char last = 0;

      if (i != s.end() && *i == '[') {
        e += *i++;
        // eat parameter bytes
        while (i != s.end() && *i >= 0x30 && *i <= 0x3f) {
          e += *i++;
        }
        // eat intermediate bytes
        while (i != s.end() && *i >= 0x20 && *i <= 0x2f) {
          e += *i++;
        }
        // eat final byte
        if (i != s.end() && *i >= 0x40 && *i <= 0x7e) {
          e += last = *i++;
        }
      } else {
        if (i != s.end() && *i >= 0x40 && *i <= 0x5f) {
          e += *i++;
        }
      }

      if (!filterAll && last == 'm') {
        t += e;
      }
    }

    else if (*i == '\t') {
      i++;
      t += ' ';
      w++;
      while (w < (size_t)width && ((w % 8) != 0u)) {
        t += ' ';
        w++;
      }
    }

    else if (*i == '\r') {
      // do nothing for now
      i++;

    } else {
      t += *i++;
      w++;
    }
  }

  return t;
}

static char base64Chars[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

std::string base64Encode(const std::string& s) {
  std::string res;
  int data = 0;
  int nbits = 0;

  for (char c : s) {
    data = data << 8 | (unsigned char)c;
    nbits += 8;
    while (nbits >= 6) {
      nbits -= 6;
      res.push_back(base64Chars[data >> nbits & 0x3f]);
    }
  }

  if (nbits != 0) {
    res.push_back(base64Chars[data << (6 - nbits) & 0x3f]);
  }
  while ((res.size() % 4) != 0u) {
    res.push_back('=');
  }

  return res;
}

std::string base64Decode(const std::string& s) {
  bool init = false;
  char decode[256];
  if (!init) {
    // FIXME: not thread-safe.
    memset(decode, -1, sizeof(decode));
    for (int i = 0; i < 64; i++) {
      decode[(int)base64Chars[i]] = i;
    }
    init = true;
  }

  std::string res;
  unsigned int d = 0;
  unsigned int bits = 0;

  for (char c : s) {
    if (c == '=') {
      break;
    }
    if (c == '\n') {
      continue;
    }

    char digit = decode[(unsigned char)c];
    if (digit == -1) {
      throw Error("invalid character in Base64 string");
    }

    bits += 6;
    d = d << 6 | digit;
    if (bits >= 8) {
      res.push_back(d >> (bits - 8) & 0xff);
      bits -= 8;
    }
  }

  return res;
}

void callFailure(const std::function<void(std::exception_ptr exc)>& failure,
                 const std::exception_ptr& exc) {
  try {
    failure(exc);
  } catch (std::exception& e) {
    LOG(ERROR) << "uncaught exception: " << e.what();
    abort();
  }
}

static Sync<std::pair<unsigned short, unsigned short>> windowSize{{0, 0}};

static void updateWindowSize() {
  struct winsize ws;
  if (ioctl(2, TIOCGWINSZ, &ws) == 0) {
    auto windowSize_(windowSize.lock());
    windowSize_->first = ws.ws_row;
    windowSize_->second = ws.ws_col;
  }
}

std::pair<unsigned short, unsigned short> getWindowSize() {
  return *windowSize.lock();
}

static Sync<std::list<std::function<void()>>> _interruptCallbacks;

static void signalHandlerThread(sigset_t set) {
  while (true) {
    int signal = 0;
    sigwait(&set, &signal);

    if (signal == SIGINT || signal == SIGTERM || signal == SIGHUP) {
      triggerInterrupt();

    } else if (signal == SIGWINCH) {
      updateWindowSize();
    }
  }
}

void triggerInterrupt() {
  _isInterrupted = true;

  {
    auto interruptCallbacks(_interruptCallbacks.lock());
    for (auto& callback : *interruptCallbacks) {
      try {
        callback();
      } catch (...) {
        ignoreException();
      }
    }
  }
}

static sigset_t savedSignalMask;

void startSignalHandlerThread() {
  updateWindowSize();

  if (sigprocmask(SIG_BLOCK, nullptr, &savedSignalMask) != 0) {
    throw SysError("quering signal mask");
  }

  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  sigaddset(&set, SIGTERM);
  sigaddset(&set, SIGHUP);
  sigaddset(&set, SIGPIPE);
  sigaddset(&set, SIGWINCH);
  if (pthread_sigmask(SIG_BLOCK, &set, nullptr) != 0) {
    throw SysError("blocking signals");
  }

  std::thread(signalHandlerThread, set).detach();
}

void restoreSignals() {
  if (sigprocmask(SIG_SETMASK, &savedSignalMask, nullptr) != 0) {
    throw SysError("restoring signals");
  }
}

/* RAII helper to automatically deregister a callback. */
struct InterruptCallbackImpl : InterruptCallback {
  std::list<std::function<void()>>::iterator it;
  ~InterruptCallbackImpl() override { _interruptCallbacks.lock()->erase(it); }
};

std::unique_ptr<InterruptCallback> createInterruptCallback(
    const std::function<void()>& callback) {
  auto interruptCallbacks(_interruptCallbacks.lock());
  interruptCallbacks->push_back(callback);

  auto res = std::make_unique<InterruptCallbackImpl>();
  res->it = interruptCallbacks->end();
  res->it--;

  return std::unique_ptr<InterruptCallback>(res.release());
}

}  // namespace nix
