#include "libstore/globals.hh"

#include <algorithm>
#include <map>
#include <thread>

#include <absl/strings/numbers.h>
#include <absl/strings/str_cat.h>
#include <absl/strings/str_split.h>
#include <dlfcn.h>

#include "libutil/archive.hh"
#include "libutil/args.hh"
#include "libutil/util.hh"
#include "nix_config.h"

namespace nix {

/* The default location of the daemon socket, relative to nixStateDir.
   The socket is in a directory to allow you to control access to the
   Nix daemon by setting the mode/ownership of the directory
   appropriately.  (This wouldn't work on the socket itself since it
   must be deleted and recreated on startup.) */
#define DEFAULT_SOCKET_PATH "/daemon-socket/socket"

Settings settings;

static GlobalConfig::Register r1(&settings);

Settings::Settings()
    : nixPrefix(NIX_PREFIX),
      nixStore(canonPath(
          getEnv("NIX_STORE_DIR", getEnv("NIX_STORE", NIX_STORE_DIR)))),
      nixDataDir(canonPath(getEnv("NIX_DATA_DIR", NIX_DATA_DIR))),
      nixLogDir(canonPath(getEnv("NIX_LOG_DIR", NIX_LOG_DIR))),
      nixStateDir(canonPath(getEnv("NIX_STATE_DIR", NIX_STATE_DIR))),
      nixConfDir(canonPath(getEnv("NIX_CONF_DIR", NIX_CONF_DIR))),
      nixLibexecDir(canonPath(getEnv("NIX_LIBEXEC_DIR", NIX_LIBEXEC_DIR))),
      nixBinDir(canonPath(getEnv("NIX_BIN_DIR", NIX_BIN_DIR))),
      nixManDir(canonPath(NIX_MAN_DIR)),
      nixDaemonSocketFile(canonPath(nixStateDir + DEFAULT_SOCKET_PATH)) {
  buildUsersGroup = getuid() == 0 ? "nixbld" : "";
  lockCPU = getEnv("NIX_AFFINITY_HACK", "1") == "1";

  caFile = getEnv("NIX_SSL_CERT_FILE", getEnv("SSL_CERT_FILE", ""));
  if (caFile.empty()) {
    for (auto& fn :
         {"/etc/ssl/certs/ca-certificates.crt",
          "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"}) {
      if (pathExists(fn)) {
        caFile = fn;
        break;
      }
    }
  }

  /* Backwards compatibility. */
  // TODO(tazjin): still?
  auto s = getEnv("NIX_REMOTE_SYSTEMS");
  if (!s.empty()) {
    Strings ss;
    for (auto p : absl::StrSplit(s, absl::ByChar(':'))) {
      ss.push_back(absl::StrCat("@", p));
    }
    builders = concatStringsSep(" ", ss);
  }

  sandboxPaths =
      absl::StrSplit("/bin/sh=" SANDBOX_SHELL, absl::ByAnyChar(" \t\n\r"));
}

void loadConfFile() {
  globalConfig.applyConfigFile(settings.nixConfDir + "/nix.conf");

  /* We only want to send overrides to the daemon, i.e. stuff from
     ~/.nix/nix.conf or the command line. */
  globalConfig.resetOverriden();

  auto dirs = getConfigDirs();
  // Iterate over them in reverse so that the ones appearing first in the path
  // take priority
  for (auto dir = dirs.rbegin(); dir != dirs.rend(); dir++) {
    globalConfig.applyConfigFile(*dir + "/nix/nix.conf");
  }
}

unsigned int Settings::getDefaultCores() {
  return std::max(1U, std::thread::hardware_concurrency());
}

StringSet Settings::getDefaultSystemFeatures() {
  /* For backwards compatibility, accept some "features" that are
     used in Nixpkgs to route builds to certain machines but don't
     actually require anything special on the machines. */
  StringSet features{"nixos-test", "benchmark", "big-parallel"};

#if __linux__
  if (access("/dev/kvm", R_OK | W_OK) == 0) {
    features.insert("kvm");
  }
#endif

  return features;
}

const std::string nixVersion = PACKAGE_VERSION;

template <>
void BaseSetting<SandboxMode>::set(const std::string& str) {
  if (str == "true") {
    value = smEnabled;
  } else if (str == "relaxed") {
    value = smRelaxed;
  } else if (str == "false") {
    value = smDisabled;
  } else {
    throw UsageError("option '%s' has invalid value '%s'", name, str);
  }
}

template <>
std::string BaseSetting<SandboxMode>::to_string() {
  if (value == smEnabled) {
    return "true";
  }
  if (value == smRelaxed) {
    return "relaxed";
  } else if (value == smDisabled) {
    return "false";
  } else {
    abort();
  }
}

template <>
void BaseSetting<SandboxMode>::toJSON(JSONPlaceholder& out) {
  AbstractSetting::toJSON(out);
}

template <>
void BaseSetting<SandboxMode>::convertToArg(Args& args,
                                            const std::string& category) {
  args.mkFlag()
      .longName(name)
      .description("Enable sandboxing.")
      .handler([=](const std::vector<std::string>& ss) { override(smEnabled); })
      .category(category);
  args.mkFlag()
      .longName("no-" + name)
      .description("Disable sandboxing.")
      .handler(
          [=](const std::vector<std::string>& ss) { override(smDisabled); })
      .category(category);
  args.mkFlag()
      .longName("relaxed-" + name)
      .description("Enable sandboxing, but allow builds to disable it.")
      .handler([=](const std::vector<std::string>& ss) { override(smRelaxed); })
      .category(category);
}

void MaxBuildJobsSetting::set(const std::string& str) {
  if (str == "auto") {
    value = std::max(1U, std::thread::hardware_concurrency());
  } else if (!absl::SimpleAtoi(str, &value)) {
    throw UsageError(
        "configuration setting '%s' should be 'auto' or an integer", name);
  }
}

}  // namespace nix
