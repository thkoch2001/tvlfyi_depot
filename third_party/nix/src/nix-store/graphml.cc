#include "nix-store/graphml.hh"

#include <iostream>

#include "libstore/derivations.hh"
#include "libstore/store-api.hh"
#include "libutil/util.hh"

using std::cout;

namespace nix {

static inline const std::string& xmlQuote(const std::string& s) {
  // Luckily, store paths shouldn't contain any character that needs to be
  // quoted.
  return s;
}

static std::string symbolicName(const std::string& path) {
  std::string p = baseNameOf(path);
  return std::string(p, p.find('-') + 1);
}

static std::string makeEdge(const std::string& src, const std::string& dst) {
  return fmt("  <edge source=\"%1%\" target=\"%2%\"/>\n", xmlQuote(src),
             xmlQuote(dst));
}

static std::string makeNode(const ValidPathInfo& info) {
  return fmt(
      "  <node id=\"%1%\">\n"
      "    <data key=\"narSize\">%2%</data>\n"
      "    <data key=\"name\">%3%</data>\n"
      "    <data key=\"type\">%4%</data>\n"
      "  </node>\n",
      info.path, info.narSize, symbolicName(info.path),
      (isDerivation(info.path) ? "derivation" : "output-path"));
}

void printGraphML(const ref<Store>& store, const PathSet& roots) {
  PathSet workList(roots);
  PathSet doneSet;
  std::pair<PathSet::iterator, bool> ret;

  cout << "<?xml version='1.0' encoding='utf-8'?>\n"
       << "<graphml xmlns='http://graphml.graphdrawing.org/xmlns'\n"
       << "    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'\n"
       << "    "
          "xsi:schemaLocation='http://graphml.graphdrawing.org/xmlns/1.0/"
          "graphml.xsd'>\n"
       << "<key id='narSize' for='node' attr.name='narSize' attr.type='int'/>"
       << "<key id='name' for='node' attr.name='name' attr.type='string'/>"
       << "<key id='type' for='node' attr.name='type' attr.type='string'/>"
       << "<graph id='G' edgedefault='directed'>\n";

  while (!workList.empty()) {
    Path path = *(workList.begin());
    workList.erase(path);

    ret = doneSet.insert(path);
    if (!ret.second) {
      continue;
    }

    ValidPathInfo info = *(store->queryPathInfo(path));
    cout << makeNode(info);

    for (auto& p : store->queryPathInfo(path)->references) {
      if (p != path) {
        workList.insert(p);
        cout << makeEdge(path, p);
      }
    }
  }

  cout << "</graph>\n";
  cout << "</graphml>\n";
}

}  // namespace nix
