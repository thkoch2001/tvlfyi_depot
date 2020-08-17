#include <regex>
#include <utility>

#include "libexpr/attr-path.hh"
#include "libexpr/common-eval-args.hh"
#include "libexpr/eval-inline.hh"
#include "libexpr/eval.hh"
#include "libexpr/get-drvs.hh"
#include "libmain/shared.hh"
#include "libstore/derivations.hh"
#include "libstore/store-api.hh"
#include "libutil/status.hh"
#include "nix/command.hh"

namespace nix {

SourceExprCommand::SourceExprCommand() {
  mkFlag()
      .shortName('f')
      .longName("file")
      .label("file")
      .description("evaluate FILE rather than the default")
      .dest(&file);
}

Value* SourceExprCommand::getSourceExpr(EvalState& state) {
  if (vSourceExpr != nullptr) {
    return *vSourceExpr;
  }

  auto sToplevel = state.symbols.Create("_toplevel");

  // Allocate the vSourceExpr Value as uncollectable. Boehm GC doesn't
  // consider the member variable "alive" during execution causing it to be
  // GC'ed in the middle of evaluation.
  vSourceExpr = allocRootValue(state.allocValue());

  if (!file.empty()) {
    state.evalFile(lookupFileArg(state, file), **vSourceExpr);
  } else {
    /* Construct the installation source from $NIX_PATH. */

    auto searchPath = state.getSearchPath();

    state.mkAttrs(**vSourceExpr, 1024);

    mkBool(*state.allocAttr(**vSourceExpr, sToplevel), true);

    std::unordered_set<std::string> seen;

    auto addEntry = [&](const std::string& name) {
      if (name.empty()) {
        return;
      }
      if (!seen.insert(name).second) {
        return;
      }
      Value* v1 = state.allocValue();
      mkPrimOpApp(*v1, state.getBuiltin("findFile"),
                  state.getBuiltin("nixPath"));
      Value* v2 = state.allocValue();
      mkApp(*v2, *v1, mkString(*state.allocValue(), name));
      mkApp(*state.allocAttr(**vSourceExpr, state.symbols.Create(name)),
            state.getBuiltin("import"), *v2);
    };

    for (auto& i : searchPath) { /* Hack to handle channels. */
      if (i.first.empty() && pathExists(i.second + "/manifest.nix")) {
        for (auto& j : readDirectory(i.second)) {
          if (j.name != "manifest.nix" &&
              pathExists(fmt("%s/%s/default.nix", i.second, j.name))) {
            addEntry(j.name);
          }
        }
      } else {
        addEntry(i.first);
      }
    }
  }

  return *vSourceExpr;
}

ref<EvalState> SourceExprCommand::getEvalState() {
  if (!evalState) {
    evalState = std::make_shared<EvalState>(searchPath, getStore());
  }
  return ref<EvalState>(evalState);
}

Buildable Installable::toBuildable() {
  auto buildables = toBuildables();
  if (buildables.size() != 1) {
    throw Error(
        "installable '%s' evaluates to %d derivations, where only one is "
        "expected",
        what(), buildables.size());
  }
  return std::move(buildables[0]);
}

struct InstallableStorePath final : Installable {
  Path storePath;

  explicit InstallableStorePath(Path storePath)
      : storePath(std::move(storePath)) {}

  std::string what() override { return storePath; }

  Buildables toBuildables() override {
    return {{isDerivation(storePath) ? storePath : "", {{"out", storePath}}}};
  }
};

struct InstallableValue : Installable {
  SourceExprCommand& cmd;

  explicit InstallableValue(SourceExprCommand& cmd) : cmd(cmd) {}

  Buildables toBuildables() override {
    auto state = cmd.getEvalState();

    auto v = toValue(*state);

    std::unique_ptr<Bindings> autoArgs = cmd.getAutoArgs(*state);

    DrvInfos drvs;
    getDerivations(*state, *v, "", autoArgs.get(), drvs, false);

    Buildables res;

    PathSet drvPaths;

    for (auto& drv : drvs) {
      Buildable b{drv.queryDrvPath()};
      drvPaths.insert(b.drvPath);

      auto outputName = drv.queryOutputName();
      if (outputName.empty()) {
        throw Error("derivation '%s' lacks an 'outputName' attribute",
                    b.drvPath);
      }

      b.outputs.emplace(outputName, drv.queryOutPath());

      res.push_back(std::move(b));
    }

    // Hack to recognize .all: if all drvs have the same drvPath,
    // merge the buildables.
    if (drvPaths.size() == 1) {
      Buildable b{*drvPaths.begin()};
      for (auto& b2 : res) {
        b.outputs.insert(b2.outputs.begin(), b2.outputs.end());
      }
      return {b};
    }
    return res;
  }
};

struct InstallableExpr final : InstallableValue {
  std::string text;

  InstallableExpr(SourceExprCommand& cmd, std::string text)
      : InstallableValue(cmd), text(std::move(text)) {}

  std::string what() override { return text; }

  Value* toValue(EvalState& state) override {
    auto v = state.allocValue();
    state.eval(state.parseExprFromString(text, absPath(".")), *v);
    return v;
  }
};

struct InstallableAttrPath final : InstallableValue {
  std::string attrPath;

  InstallableAttrPath(SourceExprCommand& cmd, std::string attrPath)
      : InstallableValue(cmd), attrPath(std::move(attrPath)) {}

  std::string what() override { return attrPath; }

  Value* toValue(EvalState& state) override {
    auto source = cmd.getSourceExpr(state);

    std::unique_ptr<Bindings> autoArgs = cmd.getAutoArgs(state);

    Value* v = findAlongAttrPath(state, attrPath, autoArgs.get(), *source);
    state.forceValue(*v);

    return v;
  }
};

// FIXME: extend
std::string attrRegex = R"([A-Za-z_][A-Za-z0-9-_+]*)";
static std::regex attrPathRegex(fmt(R"(%1%(\.%1%)*)", attrRegex));

static std::vector<std::shared_ptr<Installable>> parseInstallables(
    SourceExprCommand& cmd, const ref<Store>& store,
    std::vector<std::string> ss, bool useDefaultInstallables) {
  std::vector<std::shared_ptr<Installable>> result;

  if (ss.empty() && useDefaultInstallables) {
    if (cmd.file.empty()) {
      cmd.file = ".";
    }
    ss = {""};
  }

  for (auto& s : ss) {
    if (s.compare(0, 1, "(") == 0) {
      result.push_back(std::make_shared<InstallableExpr>(cmd, s));

    } else if (s.find('/') != std::string::npos) {
      auto path = store->toStorePath(store->followLinksToStore(s));

      if (store->isStorePath(path)) {
        result.push_back(std::make_shared<InstallableStorePath>(path));
      }
    }

    else if (s.empty() || std::regex_match(s, attrPathRegex)) {
      result.push_back(std::make_shared<InstallableAttrPath>(cmd, s));

    } else {
      throw UsageError("don't know what to do with argument '%s'", s);
    }
  }

  return result;
}

std::shared_ptr<Installable> parseInstallable(SourceExprCommand& cmd,
                                              const ref<Store>& store,
                                              const std::string& installable,
                                              bool useDefaultInstallables) {
  auto installables = parseInstallables(cmd, store, {installable}, false);
  assert(installables.size() == 1);
  return installables.front();
}

Buildables build(
    const ref<Store>& store, RealiseMode mode,
    const std::vector<std::shared_ptr<Installable>>& installables) {
  if (mode != Build) {
    settings.readOnlyMode = true;
  }

  Buildables buildables;

  PathSet pathsToBuild;

  for (auto& i : installables) {
    for (auto& b : i->toBuildables()) {
      if (!b.drvPath.empty()) {
        StringSet outputNames;
        for (auto& output : b.outputs) {
          outputNames.insert(output.first);
        }
        pathsToBuild.insert(b.drvPath + "!" +
                            concatStringsSep(",", outputNames));
      } else {
        for (auto& output : b.outputs) {
          pathsToBuild.insert(output.second);
        }
      }
      buildables.push_back(std::move(b));
    }
  }

  if (mode == DryRun) {
    printMissing(store, pathsToBuild);
  } else if (mode == Build) {
    util::OkOrThrow(store->buildPaths(pathsToBuild));
  }

  return buildables;
}

PathSet toStorePaths(
    const ref<Store>& store, RealiseMode mode,
    const std::vector<std::shared_ptr<Installable>>& installables) {
  PathSet outPaths;

  for (auto& b : build(store, mode, installables)) {
    for (auto& output : b.outputs) {
      outPaths.insert(output.second);
    }
  }

  return outPaths;
}

Path toStorePath(const ref<Store>& store, RealiseMode mode,
                 const std::shared_ptr<Installable>& installable) {
  auto paths = toStorePaths(store, mode, {installable});

  if (paths.size() != 1) {
    throw Error("argument '%s' should evaluate to one store path",
                installable->what());
  }

  return *paths.begin();
}

PathSet toDerivations(
    const ref<Store>& store,
    const std::vector<std::shared_ptr<Installable>>& installables,
    bool useDeriver) {
  PathSet drvPaths;

  for (auto& i : installables) {
    for (auto& b : i->toBuildables()) {
      if (b.drvPath.empty()) {
        if (!useDeriver) {
          throw Error("argument '%s' did not evaluate to a derivation",
                      i->what());
        }
        for (auto& output : b.outputs) {
          auto derivers = store->queryValidDerivers(output.second);
          if (derivers.empty()) {
            throw Error("'%s' does not have a known deriver", i->what());
          }
          // FIXME: use all derivers?
          drvPaths.insert(*derivers.begin());
        }
      } else {
        drvPaths.insert(b.drvPath);
      }
    }
  }

  return drvPaths;
}

void InstallablesCommand::prepare() {
  installables = parseInstallables(*this, getStore(), _installables,
                                   useDefaultInstallables());
}

void InstallableCommand::prepare() {
  installable = parseInstallable(*this, getStore(), _installable, false);
}

}  // namespace nix
