#include "nix-store/dotgraph.hh"

#include <iostream>

#include "libstore/store-api.hh"
#include "libutil/util.hh"

using std::cout;

namespace nix {

static std::string dotQuote(const std::string& s) { return "\"" + s + "\""; }

static std::string nextColour() {
  static int n = 0;
  static std::string colours[] = {"black", "red",     "green",
                                  "blue",  "magenta", "burlywood"};
  return colours[n++ % (sizeof(colours) / sizeof(std::string))];
}

static std::string makeEdge(const std::string& src, const std::string& dst) {
  format f = format("%1% -> %2% [color = %3%];\n") % dotQuote(src) %
             dotQuote(dst) % dotQuote(nextColour());
  return f.str();
}

static std::string makeNode(const std::string& id, const std::string& label,
                            const std::string& colour) {
  format f = format(
                 "%1% [label = %2%, shape = box, "
                 "style = filled, fillcolor = %3%];\n") %
             dotQuote(id) % dotQuote(label) % dotQuote(colour);
  return f.str();
}

static std::string symbolicName(const std::string& path) {
  std::string p = baseNameOf(path);
  return std::string(p, p.find('-') + 1);
}

#if 0
std::string pathLabel(const Path & nePath, const std::string & elemPath)
{
    return (std::string) nePath + "-" + elemPath;
}


void printClosure(const Path & nePath, const StoreExpr & fs)
{
    PathSet workList(fs.closure.roots);
    PathSet doneSet;

    for (PathSet::iterator i = workList.begin(); i != workList.end(); ++i) {
        cout << makeEdge(pathLabel(nePath, *i), nePath);
    }

    while (!workList.empty()) {
        Path path = *(workList.begin());
        workList.erase(path);

        if (doneSet.find(path) == doneSet.end()) {
            doneSet.insert(path);

            ClosureElems::const_iterator elem = fs.closure.elems.find(path);
            if (elem == fs.closure.elems.end())
                throw Error(format("bad closure, missing path '%1%'") % path);

            for (StringSet::const_iterator i = elem->second.refs.begin();
                 i != elem->second.refs.end(); ++i)
            {
                workList.insert(*i);
                cout << makeEdge(pathLabel(nePath, *i), pathLabel(nePath, path));
            }

            cout << makeNode(pathLabel(nePath, path),
                symbolicName(path), "#ff0000");
        }
    }
}
#endif

void printDotGraph(const ref<Store>& store, const PathSet& roots) {
  PathSet workList(roots);
  PathSet doneSet;

  cout << "digraph G {\n";

  while (!workList.empty()) {
    Path path = *(workList.begin());
    workList.erase(path);

    if (doneSet.find(path) != doneSet.end()) {
      continue;
    }
    doneSet.insert(path);

    cout << makeNode(path, symbolicName(path), "#ff0000");

    for (auto& p : store->queryPathInfo(path)->references) {
      if (p != path) {
        workList.insert(p);
        cout << makeEdge(p, path);
      }
    }

#if 0
        StoreExpr ne = storeExprFromPath(path);

        string label, colour;

        if (ne.type == StoreExpr::neDerivation) {
            for (PathSet::iterator i = ne.derivation.inputs.begin();
                 i != ne.derivation.inputs.end(); ++i)
            {
                workList.insert(*i);
                cout << makeEdge(*i, path);
            }

            label = "derivation";
            colour = "#00ff00";
            for (StringPairs::iterator i = ne.derivation.env.begin();
                 i != ne.derivation.env.end(); ++i)
                if (i->first == "name") { label = i->second; }
        }

        else if (ne.type == StoreExpr::neClosure) {
            label = "<closure>";
            colour = "#00ffff";
            printClosure(path, ne);
        }

        else abort();

        cout << makeNode(path, label, colour);
#endif
  }

  cout << "}\n";
}

}  // namespace nix
