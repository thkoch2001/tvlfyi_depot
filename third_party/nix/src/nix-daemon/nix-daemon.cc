#include <filesystem>

#include <absl/flags/flag.h>
#include <absl/flags/parse.h>
#include <absl/flags/usage_config.h>
#include <absl/strings/str_format.h>
#include <fcntl.h>
#include <glog/logging.h>
#include <grpcpp/security/server_credentials.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder_impl.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "libmain/shared.hh"  // TODO(tazjin): can this be removed?
#include "libstore/globals.hh"
#include "libstore/store-api.hh"
#include "libutil/util.hh"
#include "nix-daemon-proto.hh"
#include "nix-daemon/nix-daemon-proto.hh"
#include "nix/legacy.hh"

ABSL_FLAG(bool, pipe, false, "Use pipes for daemon communication");

namespace nix::daemon {

using grpc::Server;
using grpc_impl::ServerBuilder;
using ExternalConnectionType =
    ServerBuilder::experimental_type::ExternalConnectionType;
using NewConnectionParameters =
    grpc::experimental::ExternalConnectionAcceptor::NewConnectionParameters;

// TODO(grfn): There has to be a better way to do this - this was ported
// verbatim from the old daemon implementation without much critical evaluation.
static int ForwardToSocket(nix::Path socket_path) {
  // Forward on this connection to the real daemon
  int sockfd = socket(PF_UNIX, SOCK_STREAM, 0);
  if (sockfd == -1) {
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
  strncpy(addr.sun_path, socketName.c_str(), sizeof(addr.sun_family));

  if (connect(sockfd, reinterpret_cast<struct sockaddr*>(&addr),
              sizeof(addr)) == -1) {
    throw SysError(format("cannot connect to daemon at %1%") % socket_path);
  }

  auto nfds = (sockfd > STDIN_FILENO ? sockfd : STDIN_FILENO) + 1;
  while (true) {
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(sockfd, &fds);
    FD_SET(STDIN_FILENO, &fds);
    if (select(nfds, &fds, nullptr, nullptr, nullptr) == -1) {
      throw SysError("waiting for data from client or server");
    }
    if (FD_ISSET(sockfd, &fds)) {
      auto res = splice(sockfd, nullptr, STDOUT_FILENO, nullptr, SSIZE_MAX,
                        SPLICE_F_MOVE);
      if (res == -1) {
        throw SysError("splicing data from daemon socket to stdout");
      }
      if (res == 0) {
        throw EndOfFile("unexpected EOF from daemon socket");
      }
    }
    if (FD_ISSET(STDIN_FILENO, &fds)) {
      auto res = splice(STDIN_FILENO, nullptr, sockfd, nullptr, SSIZE_MAX,
                        SPLICE_F_MOVE);
      if (res == -1) {
        throw SysError("splicing data from stdin to daemon socket");
      }
      if (res == 0) {
        return 0;
      }
    }
  }
}

constexpr int kSdListenFdsStart = 3;

int RunServer() {
  Store::Params params;
  params["path-info-cache-size"] = "0";
  auto store = openStore(settings.storeUri, params);
  auto worker = NewWorkerService(*store);

  std::filesystem::path socket_path(settings.nixDaemonSocketFile);
  std::filesystem::create_directories(socket_path.parent_path());
  auto socket_addr = absl::StrFormat("unix://%s", socket_path);

  ServerBuilder builder;
  if (!getEnv("LISTEN_FDS").empty()) {
    // Handle systemd socket activation
    if (getEnv("LISTEN_PID") != std::to_string(getpid()) ||
        getEnv("LISTEN_FDS") != "1") {
      throw Error("Unexpected systemd environment variables");
    }
    auto socket_fd = kSdListenFdsStart;

    auto acceptor = builder.experimental().AddExternalConnectionAcceptor(
        ExternalConnectionType::FROM_FD, grpc::InsecureServerCredentials());
    for (;;) {
      try {
        struct sockaddr_un remote_addr {};
        socklen_t remote_addr_len = sizeof(remote_addr);

        int remote_fd =
            accept(socket_fd, reinterpret_cast<struct sockaddr*>(&remote_addr),
                   &remote_addr_len);
        checkInterrupt();

        if (!remote_fd) {
          if (errno == EINTR) {
            continue;
          }
          PCHECK(false) << "error accepting connection";
        }

        NewConnectionParameters p;
        p.listener_fd = socket_fd;
        p.fd = remote_fd;
        acceptor->HandleNewConnection(&p);
      } catch (Interrupted& e) {
        return -1;
      } catch (Error& e) {
        LOG(ERROR) << "error processing connection: " << e.msg();
      }
    }
  } else {
    builder.AddListeningPort(socket_addr, grpc::InsecureServerCredentials());
  }
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

}  // namespace nix::daemon

int main(int argc, char** argv) {
  FLAGS_logtostderr = true;
  google::InitGoogleLogging(argv[0]);

  absl::SetFlagsUsageConfig({.version_string = [] { return nix::nixVersion; }});
  absl::ParseCommandLine(argc, argv);

  if (absl::GetFlag(FLAGS_pipe)) {
    if (nix::getStoreType() == nix::tDaemon) {
      return nix::daemon::ForwardToSocket(nix::settings.nixDaemonSocketFile);
    } else {
      // TODO(grfn): Need to launch a server on stdin here - upstream calls
      // processConnection(true, "root", 0);
      LOG(ERROR) << "not implemented";
      return 1;
    }
  }

  return nix::daemon::RunServer();
}
