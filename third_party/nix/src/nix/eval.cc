#include "libexpr/eval.hh"

#include "libexpr/value-to-json.hh"
#include "libmain/common-args.hh"
#include "libmain/shared.hh"
#include "libstore/store-api.hh"
#include "libutil/json.hh"
#include "nix/command.hh"

using namespace nix;

struct CmdEval : MixJSON, InstallableCommand {
  bool raw = false;

  CmdEval() { mkFlag(0, "raw", "print strings unquoted", &raw); }

  std::string name() override { return "eval"; }

  std::string description() override { return "evaluate a Nix expression"; }

  Examples examples() override {
    return {
        Example{"To evaluate a Nix expression given on the command line:",
                "nix eval '(1 + 2)'"},
        Example{"To evaluate a Nix expression from a file or URI:",
                "nix eval -f channel:nixos-17.09 hello.name"},
        Example{"To get the current version of Nixpkgs:",
                "nix eval --raw nixpkgs.lib.nixpkgsVersion"},
        Example{"To print the store path of the Hello package:",
                "nix eval --raw nixpkgs.hello"},
    };
  }

  void run(ref<Store> store) override {
    if (raw && json) {
      throw UsageError("--raw and --json are mutually exclusive");
    }

    auto state = getEvalState();

    auto v = installable->toValue(*state);
    PathSet context;

    if (raw) {
      std::cout << state->coerceToString(noPos, *v, context);
    } else if (json) {
      JSONPlaceholder jsonOut(std::cout);
      printValueAsJSON(*state, true, *v, jsonOut, context);
    } else {
      state->forceValueDeep(*v);
      std::cout << *v << "\n";
    }
  }
};

static RegisterCommand r1(make_ref<CmdEval>());
