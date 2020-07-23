#include <filesystem>

#include <absl/strings/str_format.h>
#include <fcntl.h>
#include <glog/logging.h>
#include <grpcpp/security/server_credentials.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder_impl.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "libmain/shared.hh"
#include "libstore/globals.hh"
#include "libstore/store-api.hh"
#include "libutil/util.hh"
#include "nix-daemon-proto.hh"
#include "nix-daemon/nix-daemon-proto.hh"
#include "nix/legacy.hh"

namespace nix::daemon {

using grpc::Server;
using grpc_impl::ServerBuilder;

// TODO(grfn): There has to be a better way to do this - this was ported
// verbatim from the old daemon implementation without much critical evaluation.
static int ForwardToSocket(nix::Path socket_path) {
  // Forward on this connection to the real daemon
  auto s = socket(PF_UNIX, SOCK_STREAM, 0);
  if (s == -1) {
    throw SysError("creating Unix domain socket");
  }

  auto socketDir = dirOf(socket_path);
  if (chdir(socketDir.c_str()) == -1) {
    throw SysError(format("changing to socket directory '%1%'") % socketDir);
  }

  auto socketName = baseNameOf(socket_path);
  auto addr = sockaddr_un{};
  addr.sun_family = AF_UNIX;
  if (socketName.size() + 1 >= sizeof(addr.sun_path)) {
    throw Error(format("socket name %1% is too long") % socketName);
  }
  strcpy(addr.sun_path, socketName.c_str());

  if (connect(s, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
    throw SysError(format("cannot connect to daemon at %1%") % socket_path);
  }

  auto nfds = (s > STDIN_FILENO ? s : STDIN_FILENO) + 1;
  while (true) {
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(s, &fds);
    FD_SET(STDIN_FILENO, &fds);
    if (select(nfds, &fds, nullptr, nullptr, nullptr) == -1) {
      throw SysError("waiting for data from client or server");
    }
    if (FD_ISSET(s, &fds)) {
      auto res =
          splice(s, nullptr, STDOUT_FILENO, nullptr, SSIZE_MAX, SPLICE_F_MOVE);
      if (res == -1) {
        throw SysError("splicing data from daemon socket to stdout");
      }
      if (res == 0) {
        throw EndOfFile("unexpected EOF from daemon socket");
      }
    }
    if (FD_ISSET(STDIN_FILENO, &fds)) {
      auto res =
          splice(STDIN_FILENO, nullptr, s, nullptr, SSIZE_MAX, SPLICE_F_MOVE);
      if (res == -1) {
        throw SysError("splicing data from stdin to daemon socket");
      }
      if (res == 0) {
        return 0;
      }
    }
  }
}

int RunServer() {
  Store::Params params;
  params["path-info-cache-size"] = "0";
  auto store = openStore(settings.storeUri, params);
  auto worker = NewWorkerService(store);

  std::filesystem::path socket_path(settings.nixDaemonSocketFile);
  std::filesystem::create_directories(socket_path.parent_path());
  auto socket_addr = absl::StrFormat("unix://%s", socket_path);

  ServerBuilder builder;
  builder.AddListeningPort(socket_addr, grpc::InsecureServerCredentials());
  builder.RegisterService(worker);

  std::unique_ptr<Server> server(builder.BuildAndStart());
  if (server) {
    LOG(INFO) << "Nix daemon listening at " << socket_addr;
    server->Wait();
    return 0;
  } else {
    return 1;
  }
}

static int main_(int argc, char** argv) {
  auto pipe = false;

  // TODO(grfn): Replace with absl::flags
  parseCmdLine(argc, argv,
               [&](Strings::iterator& arg, const Strings::iterator& end) {
                 if (*arg == "--daemon") {
                   // NOTE(grfn): this is in fact still necessary - the current
                   // systemd unit for nix passes --daemon to the daemon. It
                   // would be nice to get rid of that eventually -  perhaps
                   // once we start distributing tvix we can do so as a nixos
                   // module that also removes the arg from the systemd unit
                 } else if (*arg == "--help") {
                   showManPage("nix-daemon");
                 } else if (*arg == "--version") {
                   printVersion("nix-daemon");
                 } else if (*arg == "--pipe") {
                   // Causes the daemon to forward stdin and stdout to and from
                   // the actual daemon socket
                   pipe = true;
                 } else {
                   return false;
                 }
                 return true;
               });

  if (pipe) {
    if (getStoreType() == tDaemon) {
      return ForwardToSocket(settings.nixDaemonSocketFile);
    } else {
      // TODO(grfn): Need to launch a server on stdin here - upstream calls
      // processConnection(true, "root", 0);
      throw "Not implemented";
    }
  }
  return RunServer();
}

// TODO(grfn): Replace this with something less magical
static RegisterLegacyCommand s1("nix-daemon", main_);

}  // namespace nix::daemon
