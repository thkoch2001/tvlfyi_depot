#include "nar-info.hh"

#include <absl/strings/numbers.h>
#include <absl/strings/str_split.h>

#include "globals.hh"

namespace nix {

NarInfo::NarInfo(const Store& store, const std::string& s,
                 const std::string& whence) {
  auto corrupt = [&]() {
    throw Error(format("NAR info file '%1%' is corrupt") % whence);
  };

  auto parseHashField = [&](const std::string& s) {
    try {
      return Hash(s);
    } catch (BadHash&) {
      corrupt();
      return Hash();  // never reached
    }
  };

  size_t pos = 0;
  while (pos < s.size()) {
    size_t colon = s.find(':', pos);
    if (colon == std::string::npos) {
      corrupt();
    }

    std::string name(s, pos, colon - pos);

    size_t eol = s.find('\n', colon + 2);
    if (eol == std::string::npos) {
      corrupt();
    }

    std::string value(s, colon + 2, eol - colon - 2);

    if (name == "StorePath") {
      if (!store.isStorePath(value)) {
        corrupt();
      }
      path = value;
    } else if (name == "URL") {
      url = value;
    } else if (name == "Compression") {
      compression = value;
    } else if (name == "FileHash") {
      fileHash = parseHashField(value);
    } else if (name == "FileSize") {
      if (!absl::SimpleAtoi(value, &fileSize)) {
        corrupt();
      }
    } else if (name == "NarHash") {
      narHash = parseHashField(value);
    } else if (name == "NarSize") {
      if (!absl::SimpleAtoi(value, &narSize)) {
        corrupt();
      }
    } else if (name == "References") {
      std::vector<std::string> refs = absl::StrSplit(value, absl::ByChar(' '));
      if (!references.empty()) {
        corrupt();
      }
      for (auto& r : refs) {
        auto r2 = store.storeDir + "/" + r;
        if (!store.isStorePath(r2)) {
          corrupt();
        }
        references.insert(r2);
      }
    } else if (name == "Deriver") {
      if (value != "unknown-deriver") {
        auto p = store.storeDir + "/" + value;
        if (!store.isStorePath(p)) {
          corrupt();
        }
        deriver = p;
      }
    } else if (name == "System") {
      system = value;
    } else if (name == "Sig") {
      sigs.insert(value);
    } else if (name == "CA") {
      if (!ca.empty()) {
        corrupt();
      }
      ca = value;
    }

    pos = eol + 1;
  }

  if (compression.empty()) {
    compression = "bzip2";
  }

  if (path.empty() || url.empty() || narSize == 0 || !narHash) {
    corrupt();
  }
}

std::string NarInfo::to_string() const {
  std::string res;
  res += "StorePath: " + path + "\n";
  res += "URL: " + url + "\n";
  assert(!compression.empty());
  res += "Compression: " + compression + "\n";
  assert(fileHash.type == htSHA256);
  res += "FileHash: " + fileHash.to_string(Base32) + "\n";
  res += "FileSize: " + std::to_string(fileSize) + "\n";
  assert(narHash.type == htSHA256);
  res += "NarHash: " + narHash.to_string(Base32) + "\n";
  res += "NarSize: " + std::to_string(narSize) + "\n";

  res += "References: " + concatStringsSep(" ", shortRefs()) + "\n";

  if (!deriver.empty()) {
    res += "Deriver: " + baseNameOf(deriver) + "\n";
  }

  if (!system.empty()) {
    res += "System: " + system + "\n";
  }

  for (const auto& sig : sigs) {
    res += "Sig: " + sig + "\n";
  }

  if (!ca.empty()) {
    res += "CA: " + ca + "\n";
  }

  return res;
}

}  // namespace nix
