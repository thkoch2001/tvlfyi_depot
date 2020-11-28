#include <absl/strings/str_split.h>
#include <glog/logging.h>
#include <unistd.h>

#include "libexpr/attr-path.hh"
#include "libexpr/eval.hh"
#include "libmain/shared.hh"
#include "nix/command.hh"

namespace nix {
struct CmdEdit final : InstallableCommand {
  std::string name() override { return "edit"; }

  std::string description() override {
    return "open the Nix expression of a Nix package in $EDITOR";
  }

  Examples examples() override {
    return {
        Example{"To open the Nix expression of the GNU Hello package:",
                "nix edit nixpkgs.hello"},
    };
  }

  void run(ref<Store> store) override {
    auto state = getEvalState();

    auto v = installable->toValue(*state);

    Value* v2;
    try {
      auto dummyArgs = Bindings::New();
      v2 = findAlongAttrPath(*state, "meta.position", dummyArgs.get(), *v);
    } catch (Error&) {
      throw Error("package '%s' has no source location information",
                  installable->what());
    }

    auto pos = state->forceString(*v2);
    DLOG(INFO) << "position is " << pos;

    auto colon = pos.rfind(':');
    if (colon == std::string::npos) {
      throw Error("cannot parse meta.position attribute '%s'", pos);
    }

    std::string filename(pos, 0, colon);
    int lineno;
    try {
      lineno = std::stoi(std::string(pos, colon + 1));
    } catch (std::invalid_argument& e) {
      throw Error("cannot parse line number '%s'", pos);
    }

    auto editor = getEnv("EDITOR").value_or("cat");

    Strings args =
        absl::StrSplit(editor, absl::ByAnyChar(" \t\n\r"), absl::SkipEmpty());

    if (editor.find("emacs") != std::string::npos ||
        editor.find("nano") != std::string::npos ||
        editor.find("vim") != std::string::npos) {
      args.push_back(fmt("+%d", lineno));
    }

    args.push_back(filename);

    execvp(args.front().c_str(), stringsToCharPtrs(args).data());

    throw SysError("cannot run editor '%s'", editor);
  }
};
}  // namespace nix

static nix::RegisterCommand r1(nix::make_ref<nix::CmdEdit>());
