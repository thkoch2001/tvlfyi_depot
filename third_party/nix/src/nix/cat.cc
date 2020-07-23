#include "libstore/fs-accessor.hh"
#include "libstore/nar-accessor.hh"
#include "libstore/store-api.hh"
#include "nix/command.hh"

using namespace nix;

struct MixCat : virtual Args {
  std::string path;

  void cat(const ref<FSAccessor>& accessor) {
    auto st = accessor->stat(path);
    if (st.type == FSAccessor::Type::tMissing) {
      throw Error(format("path '%1%' does not exist") % path);
    }
    if (st.type != FSAccessor::Type::tRegular) {
      throw Error(format("path '%1%' is not a regular file") % path);
    }

    std::cout << accessor->readFile(path);
  }
};

struct CmdCatStore final : StoreCommand, MixCat {
  CmdCatStore() { expectArg("path", &path); }

  std::string name() override { return "cat-store"; }

  std::string description() override {
    return "print the contents of a store file on stdout";
  }

  void run(std::shared_ptr<Store> store) override {
    cat(store->getFSAccessor());
  }
};

struct CmdCatNar final : StoreCommand, MixCat {
  Path narPath;

  CmdCatNar() {
    expectArg("nar", &narPath);
    expectArg("path", &path);
  }

  std::string name() override { return "cat-nar"; }

  std::string description() override {
    return "print the contents of a file inside a NAR file";
  }

  void run(std::shared_ptr<Store> store) override {
    cat(makeNarAccessor(make_ref<std::string>(readFile(narPath))));
  }
};

static RegisterCommand r1(make_ref<CmdCatStore>());
static RegisterCommand r2(make_ref<CmdCatNar>());
