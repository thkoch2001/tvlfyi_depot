#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <memory>
#include <set>
#include <tuple>

#include <absl/strings/ascii.h>
#include <absl/strings/match.h>
#include <absl/strings/str_cat.h>
#include <glog/logging.h>

#include "derivations.hh"
#include "globals.hh"
#include "legacy.hh"
#include "local-store.hh"
#include "machines.hh"
#include "pathlocks.hh"
#include "serialise.hh"
#include "shared.hh"
#include "store-api.hh"

using namespace nix;
using std::cin;

static void handleAlarm(int sig) {}

std::string escapeUri(std::string uri) {
  std::replace(uri.begin(), uri.end(), '/', '_');
  return uri;
}

static std::string currentLoad;

static AutoCloseFD openSlotLock(const Machine& m, unsigned long long slot) {
  return openLockFile(fmt("%s/%s-%d", currentLoad, escapeUri(m.storeUri), slot),
                      true);
}

static bool allSupportedLocally(const std::set<std::string>& requiredFeatures) {
  for (auto& feature : requiredFeatures) {
    if (settings.systemFeatures.get().count(feature) == 0u) {
      return false;
    }
  }
  return true;
}

static int _main(int argc, char* argv[]) {
  {
    /* Ensure we don't get any SSH passphrase or host key popups. */
    unsetenv("DISPLAY");
    unsetenv("SSH_ASKPASS");

    if (argc != 2) {
      throw UsageError("called without required arguments");
    }

    FdSource source(STDIN_FILENO);

    /* Read the parent's settings. */
    while (readInt(source) != 0u) {
      auto name = readString(source);
      auto value = readString(source);
      settings.set(name, value);
    }

    settings.maxBuildJobs.set("1");  // hack to make tests with local?root= work

    initPlugins();

    auto store = openStore().cast<LocalStore>();

    /* It would be more appropriate to use $XDG_RUNTIME_DIR, since
       that gets cleared on reboot, but it wouldn't work on macOS. */
    currentLoad = store->stateDir + "/current-load";

    std::shared_ptr<Store> sshStore;
    AutoCloseFD bestSlotLock;

    auto machines = getMachines();
    DLOG(INFO) << "got " << machines.size() << " remote builders";

    if (machines.empty()) {
      std::cerr << "# decline-permanently\n";
      return 0;
    }

    std::string drvPath;
    std::string storeUri;

    while (true) {
      try {
        auto s = readString(source);
        if (s != "try") {
          return 0;
        }
      } catch (EndOfFile&) {
        return 0;
      }

      auto amWilling = readInt(source);
      auto neededSystem = readString(source);
      source >> drvPath;
      auto requiredFeatures = readStrings<std::set<std::string>>(source);

      auto canBuildLocally =
          (amWilling != 0u) &&
          (neededSystem == settings.thisSystem ||
           settings.extraPlatforms.get().count(neededSystem) > 0) &&
          allSupportedLocally(requiredFeatures);

      /* Error ignored here, will be caught later */
      mkdir(currentLoad.c_str(), 0777);

      while (true) {
        bestSlotLock = -1;
        AutoCloseFD lock = openLockFile(currentLoad + "/main-lock", true);
        lockFile(lock.get(), ltWrite, true);

        bool rightType = false;

        Machine* bestMachine = nullptr;
        unsigned long long bestLoad = 0;
        for (auto& m : machines) {
          DLOG(INFO) << "considering building on remote machine '" << m.storeUri
                     << "'";

          if (m.enabled &&
              std::find(m.systemTypes.begin(), m.systemTypes.end(),
                        neededSystem) != m.systemTypes.end() &&
              m.allSupported(requiredFeatures) &&
              m.mandatoryMet(requiredFeatures)) {
            rightType = true;
            AutoCloseFD free;
            unsigned long long load = 0;
            for (unsigned long long slot = 0; slot < m.maxJobs; ++slot) {
              auto slotLock = openSlotLock(m, slot);
              if (lockFile(slotLock.get(), ltWrite, false)) {
                if (!free) {
                  free = std::move(slotLock);
                }
              } else {
                ++load;
              }
            }
            if (!free) {
              continue;
            }
            bool best = false;
            if (!bestSlotLock) {
              best = true;
            } else if (load / m.speedFactor <
                       bestLoad / bestMachine->speedFactor) {
              best = true;
            } else if (load / m.speedFactor ==
                       bestLoad / bestMachine->speedFactor) {
              if (m.speedFactor > bestMachine->speedFactor) {
                best = true;
              } else if (m.speedFactor == bestMachine->speedFactor) {
                if (load < bestLoad) {
                  best = true;
                }
              }
            }
            if (best) {
              bestLoad = load;
              bestSlotLock = std::move(free);
              bestMachine = &m;
            }
          }
        }

        if (!bestSlotLock) {
          if (rightType && !canBuildLocally) {
            std::cerr << "# postpone\n";
          } else {
            std::cerr << "# decline\n";
          }
          break;
        }

        futimens(bestSlotLock.get(), nullptr);

        lock = -1;

        try {
          DLOG(INFO) << "connecting to '" << bestMachine->storeUri << "'";

          Store::Params storeParams;
          if (absl::StartsWith(bestMachine->storeUri, "ssh://")) {
            storeParams["max-connections"] = "1";
            storeParams["log-fd"] = "4";
            if (!bestMachine->sshKey.empty()) {
              storeParams["ssh-key"] = bestMachine->sshKey;
            }
          }

          sshStore = openStore(bestMachine->storeUri, storeParams);
          sshStore->connect();
          storeUri = bestMachine->storeUri;

        } catch (std::exception& e) {
          auto msg = absl::StripTrailingAsciiWhitespace(drainFD(5, false));
          LOG(ERROR) << "cannot build on '" << bestMachine->storeUri
                     << "': " << e.what()
                     << (msg.empty() ? "" : absl::StrCat(": ", msg));
          bestMachine->enabled = false;
          continue;
        }

        goto connected;
      }
    }

  connected:
    close(5);

    std::cerr << "# accept\n" << storeUri << "\n";

    auto inputs = readStrings<PathSet>(source);
    auto outputs = readStrings<PathSet>(source);

    AutoCloseFD uploadLock = openLockFile(
        currentLoad + "/" + escapeUri(storeUri) + ".upload-lock", true);

    {
      DLOG(INFO) << "waiting for the upload lock to '" << storeUri << "'";

      auto old = signal(SIGALRM, handleAlarm);
      alarm(15 * 60);
      if (!lockFile(uploadLock.get(), ltWrite, true)) {
        LOG(ERROR) << "somebody is hogging the upload lock, continuing...";
      }
      alarm(0);
      signal(SIGALRM, old);
    }

    auto substitute =
        settings.buildersUseSubstitutes ? Substitute : NoSubstitute;

    {
      DLOG(INFO) << "copying dependencies to '" << storeUri << "'";
      copyPaths(store, ref<Store>(sshStore), inputs, NoRepair, NoCheckSigs,
                substitute);
    }

    uploadLock = -1;

    BasicDerivation drv(
        readDerivation(store->realStoreDir + "/" + baseNameOf(drvPath)));
    drv.inputSrcs = inputs;

    auto result = sshStore->buildDerivation(drvPath, drv);

    if (!result.success()) {
      throw Error("build of '%s' on '%s' failed: %s", drvPath, storeUri,
                  result.errorMsg);
    }

    PathSet missing;
    for (auto& path : outputs) {
      if (!store->isValidPath(path)) {
        missing.insert(path);
      }
    }

    if (!missing.empty()) {
      DLOG(INFO) << "copying outputs from '" << storeUri << "'";
      store->locksHeld.insert(missing.begin(), missing.end()); /* FIXME: ugly */
      copyPaths(ref<Store>(sshStore), store, missing, NoRepair, NoCheckSigs,
                NoSubstitute);
    }

    return 0;
  }
}

static RegisterLegacyCommand s1("build-remote", _main);
