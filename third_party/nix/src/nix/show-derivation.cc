// FIXME: integrate this with nix path-info?

#include "libmain/common-args.hh"
#include "libstore/derivations.hh"
#include "libstore/store-api.hh"
#include "libutil/archive.hh"
#include "libutil/json.hh"
#include "nix/command.hh"

namespace nix {
struct CmdShowDerivation final : InstallablesCommand {
  bool recursive = false;

  CmdShowDerivation() {
    mkFlag()
        .longName("recursive")
        .shortName('r')
        .description("include the dependencies of the specified derivations")
        .set(&recursive, true);
  }

  std::string name() override { return "show-derivation"; }

  std::string description() override {
    return "show the contents of a store derivation";
  }

  Examples examples() override {
    return {
        Example{"To show the store derivation that results from evaluating the "
                "Hello package:",
                "nix show-derivation nixpkgs.hello"},
        Example{"To show the full derivation graph (if available) that "
                "produced your NixOS system:",
                "nix show-derivation -r /run/current-system"},
    };
  }

  void run(ref<Store> store) override {
    auto drvPaths = toDerivations(store, installables, true);

    if (recursive) {
      PathSet closure;
      store->computeFSClosure(drvPaths, closure);
      drvPaths = closure;
    }

    {
      JSONObject jsonRoot(std::cout, true);

      for (auto& drvPath : drvPaths) {
        if (!isDerivation(drvPath)) {
          continue;
        }

        auto drvObj(jsonRoot.object(drvPath));

        auto drv = readDerivation(drvPath);

        {
          auto outputsObj(drvObj.object("outputs"));
          for (auto& output : drv.outputs) {
            auto outputObj(outputsObj.object(output.first));
            outputObj.attr("path", output.second.path);
            if (!output.second.hash.empty()) {
              outputObj.attr("hashAlgo", output.second.hashAlgo);
              outputObj.attr("hash", output.second.hash);
            }
          }
        }

        {
          auto inputsList(drvObj.list("inputSrcs"));
          for (auto& input : drv.inputSrcs) {
            inputsList.elem(input);
          }
        }

        {
          auto inputDrvsObj(drvObj.object("inputDrvs"));
          for (auto& input : drv.inputDrvs) {
            auto inputList(inputDrvsObj.list(input.first));
            for (auto& outputId : input.second) {
              inputList.elem(outputId);
            }
          }
        }

        drvObj.attr("platform", drv.platform);
        drvObj.attr("builder", drv.builder);

        {
          auto argsList(drvObj.list("args"));
          for (auto& arg : drv.args) {
            argsList.elem(arg);
          }
        }

        {
          auto envObj(drvObj.object("env"));
          for (auto& var : drv.env) {
            envObj.attr(var.first, var.second);
          }
        }
      }
    }

    std::cout << "\n";
  }
};
}  // namespace nix

static nix::RegisterCommand r1(nix::make_ref<nix::CmdShowDerivation>());
