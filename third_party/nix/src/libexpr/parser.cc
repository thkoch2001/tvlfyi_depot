#include "libexpr/parser.hh"

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
  if (formals->argNames.find(formal.name) != formals->argNames.end())
    throw ParseError(format("duplicate formal function argument '%1%' at %2%") %
                     formal.name % pos);
  formals->formals.push_front(formal);
  formals->argNames.insert(formal.name);
}

Expr* stripIndentation(const Pos& pos, SymbolTable& symbols,
                       std::vector<Expr*>& es) {
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
        if (e->s[j] == ' ')
          curIndent++;
        else if (e->s[j] == '\n') {
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
  std::vector<Expr*>* es2 = new std::vector<Expr*>;
  atStartOfLine = true;
  size_t curDropped = 0;
  size_t n = es.size();
  for (std::vector<Expr*>::iterator i = es.begin(); i != es.end(); ++i, --n) {
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
          if (curDropped++ >= minIndent) s2 += e->s[j];
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


}  // namespace nix
