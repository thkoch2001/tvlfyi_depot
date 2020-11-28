#include "libmain/shared.hh"
#include "libstore/store-api.hh"
#include "nix/command.hh"

namespace nix {
struct CmdPingStore final : StoreCommand {
  std::string name() override { return "ping-store"; }

  std::string description() override {
    return "test whether a store can be opened";
  }

  Examples examples() override {
    return {
        Example{
            "To test whether connecting to a remote Nix store via SSH works:",
            "nix ping-store --store ssh://mac1"},
    };
  }

  void run(ref<Store> store) override { store->connect(); }
};
}  // namespace nix

static nix::RegisterCommand r1(nix::make_ref<nix::CmdPingStore>());
