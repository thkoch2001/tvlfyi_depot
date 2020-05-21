#include "user-env.hh"

#include <glog/logging.h>

#include "derivations.hh"
#include "eval-inline.hh"
#include "eval.hh"
#include "globals.hh"
#include "profiles.hh"
#include "shared.hh"
#include "store-api.hh"
#include "util.hh"

namespace nix {

DrvInfos queryInstalled(EvalState& state, const Path& userEnv) {
  DrvInfos elems;
  Path manifestFile = userEnv + "/manifest.nix";
  if (pathExists(manifestFile)) {
    Value v;
    state.evalFile(manifestFile, v);
    Bindings& bindings(*state.allocBindings(0));
    getDerivations(state, v, "", bindings, elems, false);
  }
  return elems;
}

bool createUserEnv(EvalState& state, DrvInfos& elems, const Path& profile,
                   bool keepDerivations, const string& lockToken) {
  /* Build the components in the user environment, if they don't
     exist already. */
  PathSet drvsToBuild;
  for (auto& i : elems) {
    if (!i.queryDrvPath().empty()) {
      drvsToBuild.insert(i.queryDrvPath());
    }
  }

  DLOG(INFO) << "building user environment dependencies";
  state.store->buildPaths(drvsToBuild,
                          state.repair != 0u ? bmRepair : bmNormal);

  /* Construct the whole top level derivation. */
  PathSet references;
  Value manifest;
  state.mkList(manifest, elems.size());
  unsigned int n = 0;
  for (auto& i : elems) {
    /* Create a pseudo-derivation containing the name, system,
       output paths, and optionally the derivation path, as well
       as the meta attributes. */
    Path drvPath = keepDerivations ? i.queryDrvPath() : "";

    Value& v(*state.allocValue());
    manifest.listElems()[n++] = &v;
    state.mkAttrs(v, 16);

    mkString(*state.allocAttr(v, state.sType), "derivation");
    mkString(*state.allocAttr(v, state.sName), i.queryName());
    auto system = i.querySystem();
    if (!system.empty()) {
      mkString(*state.allocAttr(v, state.sSystem), system);
    }
    mkString(*state.allocAttr(v, state.sOutPath), i.queryOutPath());
    if (!drvPath.empty()) {
      mkString(*state.allocAttr(v, state.sDrvPath), i.queryDrvPath());
    }

    // Copy each output meant for installation.
    DrvInfo::Outputs outputs = i.queryOutputs(true);
    Value& vOutputs = *state.allocAttr(v, state.sOutputs);
    state.mkList(vOutputs, outputs.size());
    unsigned int m = 0;
    for (auto& j : outputs) {
      mkString(*(vOutputs.listElems()[m++] = state.allocValue()), j.first);
      Value& vOutputs = *state.allocAttr(v, state.symbols.Create(j.first));
      state.mkAttrs(vOutputs, 2);
      mkString(*state.allocAttr(vOutputs, state.sOutPath), j.second);

      /* This is only necessary when installing store paths, e.g.,
         `nix-env -i /nix/store/abcd...-foo'. */
      state.store->addTempRoot(j.second);
      state.store->ensurePath(j.second);

      references.insert(j.second);
    }

    // Copy the meta attributes.
    Value& vMeta = *state.allocAttr(v, state.sMeta);
    state.mkAttrs(vMeta, 16);
    StringSet metaNames = i.queryMetaNames();
    for (auto& j : metaNames) {
      Value* v = i.queryMeta(j);
      if (v == nullptr) {
        continue;
      }
      vMeta.attrs->push_back(Attr(state.symbols.Create(j), v));
    }
    vMeta.attrs->sort();
    v.attrs->sort();

    if (!drvPath.empty()) {
      references.insert(drvPath);
    }
  }

  /* Also write a copy of the list of user environment elements to
     the store; we need it for future modifications of the
     environment. */
  Path manifestFile = state.store->addTextToStore(
      "env-manifest.nix", (format("%1%") % manifest).str(), references);

  /* Get the environment builder expression. */
  Value envBuilder;
  state.evalFile(state.findFile("nix/buildenv.nix"), envBuilder);

  /* Construct a Nix expression that calls the user environment
     builder with the manifest as argument. */
  Value args;
  Value topLevel;
  state.mkAttrs(args, 3);
  mkString(*state.allocAttr(args, state.symbols.Create("manifest")),
           manifestFile, {manifestFile});
  args.attrs->push_back(Attr(state.symbols.Create("derivations"), &manifest));
  args.attrs->sort();
  mkApp(topLevel, envBuilder, args);

  /* Evaluate it. */
  DLOG(INFO) << "evaluating user environment builder";
  state.forceValue(topLevel);
  PathSet context;
  Attr& aDrvPath(*topLevel.attrs->find(state.sDrvPath));
  Path topLevelDrv =
      state.coerceToPath(aDrvPath.pos != nullptr ? *(aDrvPath.pos) : noPos,
                         *(aDrvPath.value), context);
  Attr& aOutPath(*topLevel.attrs->find(state.sOutPath));
  Path topLevelOut =
      state.coerceToPath(aOutPath.pos != nullptr ? *(aOutPath.pos) : noPos,
                         *(aOutPath.value), context);

  /* Realise the resulting store expression. */
  DLOG(INFO) << "building user environment";
  state.store->buildPaths({topLevelDrv},
                          state.repair != 0u ? bmRepair : bmNormal);

  /* Switch the current user environment to the output path. */
  auto store2 = state.store.dynamic_pointer_cast<LocalFSStore>();

  if (store2) {
    PathLocks lock;
    lockProfile(lock, profile);

    Path lockTokenCur = optimisticLockProfile(profile);
    if (lockToken != lockTokenCur) {
      LOG(WARNING) << "profile '" << profile
                   << "' changed while we were busy; restarting";
      return false;
    }

    DLOG(INFO) << "switching to new user environment";
    Path generation =
        createGeneration(ref<LocalFSStore>(store2), profile, topLevelOut);
    switchLink(profile, generation);
  }

  return true;
}

}  // namespace nix
