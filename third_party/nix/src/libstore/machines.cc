#include "libstore/machines.hh"

#include <algorithm>

#include <absl/strings/ascii.h>
#include <absl/strings/match.h>
#include <absl/strings/str_split.h>
#include <absl/strings/string_view.h>
#include <glog/logging.h>

#include "libstore/globals.hh"
#include "libutil/util.hh"

namespace nix {

Machine::Machine(decltype(storeUri)& storeUri,
                 decltype(systemTypes)& systemTypes, decltype(sshKey)& sshKey,
                 decltype(maxJobs) maxJobs, decltype(speedFactor) speedFactor,
                 decltype(supportedFeatures)& supportedFeatures,
                 decltype(mandatoryFeatures)& mandatoryFeatures,
                 decltype(sshPublicHostKey)& sshPublicHostKey)
    : storeUri(
          // Backwards compatibility: if the URI is a hostname,
          // prepend ssh://.
          storeUri.find("://") != std::string::npos ||
                  absl::StartsWith(storeUri, "local") ||
                  absl::StartsWith(storeUri, "remote") ||
                  absl::StartsWith(storeUri, "auto") ||
                  absl::StartsWith(storeUri, "/")
              ? storeUri
              : "ssh://" + storeUri),
      systemTypes(systemTypes),
      sshKey(sshKey),
      maxJobs(maxJobs),
      speedFactor(std::max(1U, speedFactor)),
      supportedFeatures(supportedFeatures),
      mandatoryFeatures(mandatoryFeatures),
      sshPublicHostKey(sshPublicHostKey) {}

bool Machine::allSupported(const std::set<std::string>& features) const {
  return std::all_of(features.begin(), features.end(),
                     [&](const std::string& feature) {
                       return (supportedFeatures.count(feature) != 0u) ||
                              (mandatoryFeatures.count(feature) != 0u);
                     });
}

bool Machine::mandatoryMet(const std::set<std::string>& features) const {
  return std::all_of(
      mandatoryFeatures.begin(), mandatoryFeatures.end(),
      [&](const std::string& feature) { return features.count(feature); });
}

void parseMachines(const std::string& s, Machines& machines) {
  for (auto line : absl::StrSplit(s, absl::ByAnyChar("\n;"))) {
    // Skip empty lines & comments
    line = absl::StripAsciiWhitespace(line);
    if (line.empty() || line[line.find_first_not_of(" \t")] == '#') {
      continue;
    }

    if (line[0] == '@') {
      auto file = absl::StripAsciiWhitespace(line.substr(1));
      try {
        parseMachines(readFile(file), machines);
      } catch (const SysError& e) {
        if (e.errNo != ENOENT) {
          throw;
        }
        DLOG(INFO) << "cannot find machines file: " << file;
      }
      continue;
    }

    std::vector<std::string> tokens =
        absl::StrSplit(line, absl::ByAnyChar(" \t\n\r"));
    auto sz = tokens.size();
    if (sz < 1) {
      throw FormatError("bad machine specification '%s'", line);
    }

    auto isSet = [&](size_t n) {
      return tokens.size() > n && !tokens[n].empty() && tokens[n] != "-";
    };

    // TODO(tazjin): what???
    machines.emplace_back(
        tokens[0],
        isSet(1) ? absl::StrSplit(tokens[1], absl::ByChar(','))
                 : std::vector<std::string>{settings.thisSystem},
        isSet(2) ? tokens[2] : "", isSet(3) ? std::stoull(tokens[3]) : 1LL,
        isSet(4) ? std::stoull(tokens[4]) : 1LL,
        isSet(5) ? absl::StrSplit(tokens[5], absl::ByChar(','))
                 : std::set<std::string>{},
        isSet(6) ? absl::StrSplit(tokens[6], absl::ByChar(','))
                 : std::set<std::string>{},
        isSet(7) ? tokens[7] : "");
  }
}

Machines getMachines() {
  static auto machines = [&]() {
    Machines machines;
    parseMachines(settings.builders, machines);
    return machines;
  }();
  return machines;
}

}  // namespace nix
