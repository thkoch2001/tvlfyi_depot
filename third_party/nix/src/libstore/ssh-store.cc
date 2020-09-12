#include <absl/strings/str_cat.h>

#include "libstore/remote-fs-accessor.hh"
#include "libstore/remote-store.hh"
#include "libstore/ssh.hh"
#include "libstore/store-api.hh"
#include "libstore/worker-protocol.hh"
#include "libutil/archive.hh"
#include "libutil/pool.hh"

namespace nix {

constexpr std::string_view kUriScheme = "ssh-ng://";

class SSHStore : public RemoteStore {
 public:
  const Setting<Path> sshKey{(Store*)this, "", "ssh-key",
                             "path to an SSH private key"};
  const Setting<bool> compress{(Store*)this, false, "compress",
                               "whether to compress the connection"};

  SSHStore(const std::string& host, const Params& params)
      : Store(params),
        RemoteStore(params),
        host(host),
        master(host, sshKey,
               // Use SSH master only if using more than 1 connection.
               connections->capacity() > 1, compress) {}

  std::string getUri() override { return absl::StrCat(kUriScheme, host); }

  bool sameMachine() override { return false; }

  void narFromPath(const Path& path, Sink& sink) override;

  ref<FSAccessor> getFSAccessor() override;

 private:
  struct Connection : RemoteStore::Connection {
    std::unique_ptr<SSHMaster::Connection> sshConn;
  };

  ref<RemoteStore::Connection> openConnection() override;

  std::string host;

  SSHMaster master;

  void setOptions(RemoteStore::Connection& conn) override{
      /* TODO Add a way to explicitly ask for some options to be
         forwarded. One option: A way to query the daemon for its
         settings, and then a series of params to SSHStore like
         forward-cores or forward-overridden-cores that only
         override the requested settings.
      */
  };
};

void SSHStore::narFromPath(const Path& path, Sink& sink) {
  auto conn(connections->get());
  conn->to << wopNarFromPath << path;
  conn->processStderr();
  copyNAR(conn->from, sink);
}

ref<FSAccessor> SSHStore::getFSAccessor() {
  return make_ref<RemoteFSAccessor>(ref<Store>(shared_from_this()));
}

ref<RemoteStore::Connection> SSHStore::openConnection() {
  auto conn = make_ref<Connection>();
  conn->sshConn = master.startCommand("nix-daemon --pipe");
  conn->to = FdSink(conn->sshConn->in.get());
  conn->from = FdSource(conn->sshConn->out.get());
  initConnection(*conn);
  return conn;
}

static RegisterStoreImplementation regStore(
    [](const std::string& uri,
       const Store::Params& params) -> std::shared_ptr<Store> {
      if (std::string(uri, 0, kUriScheme.size()) != kUriScheme) {
        return nullptr;
      }
      return std::make_shared<SSHStore>(std::string(uri, kUriScheme.size()),
                                        params);
    });

}  // namespace nix
