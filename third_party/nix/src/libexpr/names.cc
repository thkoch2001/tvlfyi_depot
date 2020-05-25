#include "names.hh"

#include <memory>

#include <absl/strings/numbers.h>

#include "util.hh"

namespace nix {

DrvName::DrvName() { name = ""; }

/* Parse a derivation name.  The `name' part of a derivation name is
   everything up to but not including the first dash *not* followed by
   a letter.  The `version' part is the rest (excluding the separating
   dash).  E.g., `apache-httpd-2.0.48' is parsed to (`apache-httpd',
   '2.0.48'). */
DrvName::DrvName(const std::string& s) : hits(0) {
  name = fullName = s;
  for (unsigned int i = 0; i < s.size(); ++i) {
    /* !!! isalpha/isdigit are affected by the locale. */
    if (s[i] == '-' && i + 1 < s.size() && (isalpha(s[i + 1]) == 0)) {
      name = std::string(s, 0, i);
      version = std::string(s, i + 1);
      break;
    }
  }
}

bool DrvName::matches(DrvName& n) {
  if (name != "*") {
    if (!regex) {
      regex = std::make_unique<std::regex>(name, std::regex::extended);
    }
    if (!std::regex_match(n.name, *regex)) {
      return false;
    }
  }
  return !(!version.empty() && version != n.version);
}

std::string nextComponent(std::string::const_iterator& p,
                          const std::string::const_iterator end) {
  /* Skip any dots and dashes (component separators). */
  while (p != end && (*p == '.' || *p == '-')) {
    ++p;
  }

  if (p == end) {
    return "";
  }

  /* If the first character is a digit, consume the longest sequence
     of digits.  Otherwise, consume the longest sequence of
     non-digit, non-separator characters. */
  std::string s;
  if (isdigit(*p) != 0) {
    while (p != end && (isdigit(*p) != 0)) {
      s += *p++;
    }
  } else {
    while (p != end && ((isdigit(*p) == 0) && *p != '.' && *p != '-')) {
      s += *p++;
    }
  }

  return s;
}

static bool componentsLT(const std::string& c1, const std::string& c2) {
  int n1;
  int n2;
  bool c1Num = absl::SimpleAtoi(c1, &n1);
  bool c2Num = absl::SimpleAtoi(c2, &n2);

  if (c1Num && c2Num) {
    return n1 < n2;
  }
  if (c1.empty() && c2Num) {
    return true;
  } else if (c1 == "pre" && c2 != "pre") {
    return true;
  } else if (c2 == "pre") {
    return false;
    /* Assume that `2.3a' < `2.3.1'. */
  } else if (c2Num) {
    return true;
  } else if (c1Num) {
    return false;
  } else {
    return c1 < c2;
  }
}

int compareVersions(const std::string& v1, const std::string& v2) {
  std::string::const_iterator p1 = v1.begin();
  std::string::const_iterator p2 = v2.begin();

  while (p1 != v1.end() || p2 != v2.end()) {
    std::string c1 = nextComponent(p1, v1.end());
    std::string c2 = nextComponent(p2, v2.end());
    if (componentsLT(c1, c2)) {
      return -1;
    }
    if (componentsLT(c2, c1)) {
      return 1;
    }
  }

  return 0;
}

DrvNames drvNamesFromArgs(const Strings& opArgs) {
  DrvNames result;
  for (auto& i : opArgs) {
    result.push_back(DrvName(i));
  }
  return result;
}

}  // namespace nix
