#include "libexpr/parser.hh"

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "libexpr/eval.hh"
#include "libstore/download.hh"
#include "libstore/store-api.hh"

namespace nix {

void addAttr(ExprAttrs* attrs, AttrPath& attrPath, Expr* e, const Pos& pos) {
  AttrPath::iterator i;
  // All attrpaths have at least one attr
  assert(!attrPath.empty());
  // Checking attrPath validity.
  // ===========================
  for (i = attrPath.begin(); i + 1 < attrPath.end(); i++) {
    if (const auto* sym = std::get_if<Symbol>(&(*i)); sym && sym->set()) {
      ExprAttrs::AttrDefs::iterator j = attrs->attrs.find(*sym);
      if (j != attrs->attrs.end()) {
        if (!j->second.inherited) {
          ExprAttrs* attrs2 = dynamic_cast<ExprAttrs*>(j->second.e);
          if (!attrs2) {
            dupAttr(attrPath, pos, j->second.pos);
          }
          attrs = attrs2;
        } else {
          dupAttr(attrPath, pos, j->second.pos);
        }
      } else {
        ExprAttrs* nested = new ExprAttrs;
        attrs->attrs[*sym] = ExprAttrs::AttrDef(nested, pos);
        attrs = nested;
      }
    } else {
      // Yes, this code does not handle all conditions
      // exhaustively. We use std::get to throw if the condition
      // that isn't covered happens, which is potentially a
      // behaviour change from the previous default constructed
      // Symbol. It should alert us about anything untoward going
      // on here.
      auto* expr = std::get<Expr*>(*i);

      ExprAttrs* nested = new ExprAttrs;
      attrs->dynamicAttrs.push_back(
          ExprAttrs::DynamicAttrDef(expr, nested, pos));
      attrs = nested;
    }
  }
  // Expr insertion.
  // ==========================
  if (auto* sym = std::get_if<Symbol>(&(*i)); sym && sym->set()) {
    ExprAttrs::AttrDefs::iterator j = attrs->attrs.find(*sym);
    if (j != attrs->attrs.end()) {
      // This attr path is already defined. However, if both
      // e and the expr pointed by the attr path are two attribute sets,
      // we want to merge them.
      // Otherwise, throw an error.
      auto ae = dynamic_cast<ExprAttrs*>(e);
      auto jAttrs = dynamic_cast<ExprAttrs*>(j->second.e);
      if (jAttrs && ae) {
        for (auto& ad : ae->attrs) {
          auto j2 = jAttrs->attrs.find(ad.first);
          if (j2 !=
              jAttrs->attrs.end()) {  // Attr already defined in iAttrs, error.
            dupAttr(ad.first, j2->second.pos, ad.second.pos);
          }
          jAttrs->attrs[ad.first] = ad.second;
        }
      } else {
        dupAttr(attrPath, pos, j->second.pos);
      }
    } else {
      // This attr path is not defined. Let's create it.
      attrs->attrs[*sym] = ExprAttrs::AttrDef(e, pos);
    }
  } else {
    // Same caveat as the identical line above.
    auto* expr = std::get<Expr*>(*i);
    attrs->dynamicAttrs.push_back(ExprAttrs::DynamicAttrDef(expr, e, pos));
  }
}

void addFormal(const Pos& pos, Formals* formals, const Formal& formal) {
  if (formals->argNames.find(formal.name) != formals->argNames.end()) {
    throw ParseError(format("duplicate formal function argument '%1%' at %2%") %
                     formal.name % pos);
  }
  formals->formals.push_front(formal);
  formals->argNames.insert(formal.name);
}

Expr* stripIndentation(const Pos& pos, SymbolTable& symbols, VectorExprs& es) {
  if (es.empty()) {
    return new ExprString(symbols.Create(""));
  }

  /* Figure out the minimum indentation.  Note that by design
     whitespace-only final lines are not taken into account.  (So
     the " " in "\n ''" is ignored, but the " " in "\n foo''" is.) */
  bool atStartOfLine = true; /* = seen only whitespace in the current line */
  size_t minIndent = 1000000;
  size_t curIndent = 0;
  for (auto& i : es) {
    ExprIndStr* e = dynamic_cast<ExprIndStr*>(i);
    if (!e) {
      /* Anti-quotations end the current start-of-line whitespace. */
      if (atStartOfLine) {
        atStartOfLine = false;
        if (curIndent < minIndent) {
          minIndent = curIndent;
        }
      }
      continue;
    }
    for (size_t j = 0; j < e->s.size(); ++j) {
      if (atStartOfLine) {
        if (e->s[j] == ' ') {
          curIndent++;
        } else if (e->s[j] == '\n') {
          /* Empty line, doesn't influence minimum
             indentation. */
          curIndent = 0;
        } else {
          atStartOfLine = false;
          if (curIndent < minIndent) {
            minIndent = curIndent;
          }
        }
      } else if (e->s[j] == '\n') {
        atStartOfLine = true;
        curIndent = 0;
      }
    }
  }

  /* Strip spaces from each line. */
  VectorExprs* es2 = new VectorExprs;
  atStartOfLine = true;
  size_t curDropped = 0;
  size_t n = es.size();
  for (VectorExprs::iterator i = es.begin(); i != es.end(); ++i, --n) {
    ExprIndStr* e = dynamic_cast<ExprIndStr*>(*i);
    if (!e) {
      atStartOfLine = false;
      curDropped = 0;
      es2->push_back(*i);
      continue;
    }

    std::string s2;
    for (size_t j = 0; j < e->s.size(); ++j) {
      if (atStartOfLine) {
        if (e->s[j] == ' ') {
          if (curDropped++ >= minIndent) {
            s2 += e->s[j];
          }
        } else if (e->s[j] == '\n') {
          curDropped = 0;
          s2 += e->s[j];
        } else {
          atStartOfLine = false;
          curDropped = 0;
          s2 += e->s[j];
        }
      } else {
        s2 += e->s[j];
        if (e->s[j] == '\n') {
          atStartOfLine = true;
        }
      }
    }

    /* Remove the last line if it is empty and consists only of
       spaces. */
    if (n == 1) {
      std::string::size_type p = s2.find_last_of('\n');
      if (p != std::string::npos &&
          s2.find_first_not_of(' ', p + 1) == std::string::npos) {
        s2 = std::string(s2, 0, p + 1);
      }
    }

    es2->push_back(new ExprString(symbols.Create(s2)));
  }

  /* If this is a single string, then don't do a concatenation. */
  return es2->size() == 1 && dynamic_cast<ExprString*>((*es2)[0])
             ? (*es2)[0]
             : new ExprConcatStrings(pos, true, es2);
}

Path resolveExprPath(Path path) {
  assert(path[0] == '/');

  /* If `path' is a symlink, follow it.  This is so that relative
     path references work. */
  struct stat st;
  while (true) {
    if (lstat(path.c_str(), &st)) {
      throw SysError(format("getting status of '%1%'") % path);
    }
    if (!S_ISLNK(st.st_mode)) {
      break;
    }
    path = absPath(readLink(path), dirOf(path));
  }

  /* If `path' refers to a directory, append `/default.nix'. */
  if (S_ISDIR(st.st_mode)) {
    path = canonPath(path + "/default.nix");
  }

  return path;
}

// These methods are actually declared in eval.hh, and were - for some
// reason - previously implemented in parser.y.

Expr* EvalState::parseExprFromFile(const Path& path) {
  return parseExprFromFile(path, staticBaseEnv);
}

Expr* EvalState::parseExprFromFile(const Path& path, StaticEnv& staticEnv) {
  return parse(readFile(path).c_str(), path, dirOf(path), staticEnv);
}

Expr* EvalState::parseExprFromString(const std::string& s, const Path& basePath,
                                     StaticEnv& staticEnv) {
  return parse(s.c_str(), "(std::string)", basePath, staticEnv);
}

Expr* EvalState::parseExprFromString(const std::string& s,
                                     const Path& basePath) {
  return parseExprFromString(s, basePath, staticBaseEnv);
}

Expr* EvalState::parseStdin() {
  // Activity act(*logger, lvlTalkative, format("parsing standard input"));
  return parseExprFromString(drainFD(0), absPath("."));
}

void EvalState::addToSearchPath(const std::string& s) {
  size_t pos = s.find('=');
  std::string prefix;
  Path path;
  if (pos == std::string::npos) {
    path = s;
  } else {
    prefix = std::string(s, 0, pos);
    path = std::string(s, pos + 1);
  }

  searchPath.emplace_back(prefix, path);
}

Path EvalState::findFile(const std::string& path) {
  return findFile(searchPath, path);
}

Path EvalState::findFile(SearchPath& searchPath, const std::string& path,
                         const Pos& pos) {
  for (auto& i : searchPath) {
    std::string suffix;
    if (i.first.empty()) {
      suffix = "/" + path;
    } else {
      auto s = i.first.size();
      if (path.compare(0, s, i.first) != 0 ||
          (path.size() > s && path[s] != '/')) {
        continue;
      }
      suffix = path.size() == s ? "" : "/" + std::string(path, s);
    }
    auto r = resolveSearchPathElem(i);
    if (!r.first) {
      continue;
    }
    Path res = r.second + suffix;
    if (pathExists(res)) {
      return canonPath(res);
    }
  }
  format f = format(
      "file '%1%' was not found in the Nix search path (add it using $NIX_PATH "
      "or -I)" +
      std::string(pos ? ", at %2%" : ""));
  f.exceptions(boost::io::all_error_bits ^ boost::io::too_many_args_bit);
  throw ThrownError(f % path % pos);
}

std::pair<bool, std::string> EvalState::resolveSearchPathElem(
    const SearchPathElem& elem) {
  auto i = searchPathResolved.find(elem.second);
  if (i != searchPathResolved.end()) {
    return i->second;
  }

  std::pair<bool, std::string> res;

  if (isUri(elem.second)) {
    try {
      CachedDownloadRequest request(elem.second);
      request.unpack = true;
      res = {true, getDownloader()->downloadCached(store, request).path};
    } catch (DownloadError& e) {
      LOG(WARNING) << "Nix search path entry '" << elem.second
                   << "' cannot be downloaded, ignoring";
      res = {false, ""};
    }
  } else {
    auto path = absPath(elem.second);
    if (pathExists(path)) {
      res = {true, path};
    } else {
      LOG(WARNING) << "Nix search path entry '" << elem.second
                   << "' does not exist, ignoring";
      res = {false, ""};
    }
  }

  VLOG(2) << "resolved search path element '" << elem.second << "' to '"
          << res.second << "'";

  searchPathResolved[elem.second] = res;
  return res;
}

}  // namespace nix
