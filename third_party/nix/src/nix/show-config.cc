#include "libmain/common-args.hh"
#include "libmain/shared.hh"
#include "libstore/store-api.hh"
#include "libutil/json.hh"
#include "nix/command.hh"

using namespace nix;

struct CmdShowConfig : Command, MixJSON {
  CmdShowConfig() = default;

  std::string name() override { return "show-config"; }

  std::string description() override { return "show the Nix configuration"; }

  void run() override {
    if (json) {
      // FIXME: use appropriate JSON types (bool, ints, etc).
      JSONObject jsonObj(std::cout);
      globalConfig.toJSON(jsonObj);
    } else {
      std::map<std::string, Config::SettingInfo> settings;
      globalConfig.getSettings(settings);
      for (auto& s : settings) {
        std::cout << s.first + " = " + s.second.value + "\n";
      }
    }
  }
};

static RegisterCommand r1(make_ref<CmdShowConfig>());
