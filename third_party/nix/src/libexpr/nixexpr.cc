#include "libexpr/nixexpr.hh"

#include <cstdlib>
#include <variant>

#include "libstore/derivations.hh"
#include "libutil/util.hh"
#include "libutil/visitor.hh"

namespace nix {

/* Displaying abstract syntax trees. */

std::ostream& operator<<(std::ostream& str, const Expr& e) {
  e.show(str);
  return str;
}

static void showString(std::ostream& str, const std::string& s) {
  str << '"';
  for (auto c : std::string(s)) {
    if (c == '"' || c == '\\' || c == '$') {
      str << "\\" << c;
    } else if (c == '\n') {
      str << "\\n";
    } else if (c == '\r') {
      str << "\\r";
    } else if (c == '\t') {
      str << "\\t";
    } else {
      str << c;
    }
  }
  str << '"';
}

static void showId(std::ostream& str, const std::string& s) {
  if (s.empty()) {
    str << "\"\"";
  } else if (s == "if") {  // FIXME: handle other keywords
    str << '"' << s << '"';
  } else {
    char c = s[0];
    if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_')) {
      showString(str, s);
      return;
    }
    for (auto c : s) {
      if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') || c == '_' || c == '\'' || c == '-')) {
        showString(str, s);
        return;
      }
    }
    str << s;
  }
}

std::ostream& operator<<(std::ostream& str, const Symbol& sym) {
  showId(str, *sym.s);
  return str;
}

void Expr::show(std::ostream& str) const { abort(); }

void ExprInt::show(std::ostream& str) const { str << n; }

void ExprFloat::show(std::ostream& str) const { str << nf; }

void ExprString::show(std::ostream& str) const { showString(str, s); }

void ExprPath::show(std::ostream& str) const { str << s; }

void ExprVar::show(std::ostream& str) const { str << name; }

void ExprSelect::show(std::ostream& str) const {
  str << "(" << *e << ")." << showAttrPath(attrPath);
  if (def != nullptr) {
    str << " or (" << *def << ")";
  }
}

void ExprOpHasAttr::show(std::ostream& str) const {
  str << "((" << *e << ") ? " << showAttrPath(attrPath) << ")";
}

void ExprAttrs::show(std::ostream& str) const {
  if (recursive) {
    str << "rec ";
  }
  str << "{ ";
  for (auto& i : attrs) {
    if (i.second.inherited) {
      str << "inherit " << i.first << " "
          << "; ";
    } else {
      str << i.first << " = " << *i.second.e << "; ";
    }
  }
  for (auto& i : dynamicAttrs) {
    str << "\"${" << *i.nameExpr << "}\" = " << *i.valueExpr << "; ";
  }
  str << "}";
}

void ExprList::show(std::ostream& str) const {
  str << "[ ";
  for (auto& i : elems) {
    str << "(" << *i << ") ";
  }
  str << "]";
}

void ExprLambda::show(std::ostream& str) const {
  str << "(";
  if (matchAttrs) {
    str << "{ ";
    bool first = true;
    for (auto& i : formals->formals) {
      if (first) {
        first = false;
      } else {
        str << ", ";
      }
      str << i.name;
      if (i.def != nullptr) {
        str << " ? " << *i.def;
      }
    }
    if (formals->ellipsis) {
      if (!first) {
        str << ", ";
      }
      str << "...";
    }
    str << " }";
    if (!arg.empty()) {
      str << " @ ";
    }
  }
  if (!arg.empty()) {
    str << arg;
  }
  str << ": " << *body << ")";
}

void ExprLet::show(std::ostream& str) const {
  str << "(let ";
  for (auto& i : attrs->attrs) {
    if (i.second.inherited) {
      str << "inherit " << i.first << "; ";
    } else {
      str << i.first << " = " << *i.second.e << "; ";
    }
  }
  str << "in " << *body << ")";
}

void ExprWith::show(std::ostream& str) const {
  str << "(with " << *attrs << "; " << *body << ")";
}

void ExprIf::show(std::ostream& str) const {
  str << "(if " << *cond << " then " << *then << " else " << *else_ << ")";
}

void ExprAssert::show(std::ostream& str) const {
  str << "assert " << *cond << "; " << *body;
}

void ExprOpNot::show(std::ostream& str) const { str << "(! " << *e << ")"; }

void ExprConcatStrings::show(std::ostream& str) const {
  bool first = true;
  str << "(";
  for (auto& i : *es) {
    if (first) {
      first = false;
    } else {
      str << " + ";
    }
    str << *i;
  }
  str << ")";
}

void ExprPos::show(std::ostream& str) const { str << "__curPos"; }

std::ostream& operator<<(std::ostream& str, const Pos& pos) {
  if (!pos || !pos.file.has_value()) {
    str << "undefined position";
  } else {
    str << (format(ANSI_BOLD "%1%" ANSI_NORMAL ":%2%:%3%") %
            std::string(pos.file.value()) % pos.line % pos.column)
               .str();
  }
  return str;
}

std::string showAttrPath(const AttrPath& attrPath) {
  std::ostringstream out;
  bool first = true;
  for (auto& attr : attrPath) {
    if (!first) {
      out << '.';
    } else {
      first = false;
    }

    std::visit(util::overloaded{
                   [&](const Symbol& sym) { out << sym; },
                   [&](const Expr* expr) { out << "\"${" << *expr << "}\""; }},
               attr);
  }
  return out.str();
}

Pos noPos;

/* Computing levels/displacements for variables. */

void Expr::bindVars(const StaticEnv& env) { abort(); }

void ExprInt::bindVars(const StaticEnv& env) {}

void ExprFloat::bindVars(const StaticEnv& env) {}

void ExprString::bindVars(const StaticEnv& env) {}

void ExprPath::bindVars(const StaticEnv& env) {}

void ExprVar::bindVars(const StaticEnv& env) {
  /* Check whether the variable appears in the environment.  If so,
     set its level and displacement. */
  const StaticEnv* curEnv;
  unsigned int level;
  std::optional<unsigned int> withLevel = std::nullopt;
  for (curEnv = &env, level = 0; curEnv != nullptr;
       curEnv = curEnv->up, level++) {
    if (curEnv->isWith) {
      if (!withLevel.has_value()) {
        withLevel = level;
      }
    } else {
      auto i = curEnv->vars.find(name);
      if (i != curEnv->vars.end()) {
        fromWith = false;
        this->level = level;
        displ = i->second;
        return;
      }
    }
  }

  /* Otherwise, the variable must be obtained from the nearest
     enclosing `with'.  If there is no `with', then we can issue an
     "undefined variable" error now. */
  if (!withLevel.has_value()) {
    throw UndefinedVarError(format("undefined variable '%1%' at %2%") % name %
                            pos);
  }

  fromWith = true;
  this->level = withLevel.value();
}

void ExprSelect::bindVars(const StaticEnv& env) {
  e->bindVars(env);
  if (def != nullptr) {
    def->bindVars(env);
  }
  for (auto& i : attrPath) {
    if (auto* expr = std::get_if<Expr*>(&i)) {
      (*expr)->bindVars(env);
    }
  }
}

void ExprOpHasAttr::bindVars(const StaticEnv& env) {
  e->bindVars(env);
  for (auto& i : attrPath) {
    if (auto* expr = std::get_if<Expr*>(&i)) {
      (*expr)->bindVars(env);
    }
  }
}

void ExprAttrs::bindVars(const StaticEnv& env) {
  const StaticEnv* dynamicEnv = &env;
  StaticEnv newEnv(/* isWith = */ false, &env);

  if (recursive) {
    dynamicEnv = &newEnv;

    unsigned int displ = 0;
    for (auto& i : attrs) {
      newEnv.vars[i.first] = i.second.displ = displ++;
    }

    for (auto& i : attrs) {
      i.second.e->bindVars(i.second.inherited ? env : newEnv);
    }
  }

  else {
    for (auto& i : attrs) {
      i.second.e->bindVars(env);
    }
  }

  for (auto& i : dynamicAttrs) {
    i.nameExpr->bindVars(*dynamicEnv);
    i.valueExpr->bindVars(*dynamicEnv);
  }
}

void ExprList::bindVars(const StaticEnv& env) {
  for (auto& i : elems) {
    i->bindVars(env);
  }
}

void ExprLambda::bindVars(const StaticEnv& env) {
  StaticEnv newEnv(false, &env);

  unsigned int displ = 0;

  if (!arg.empty()) {
    newEnv.vars[arg] = displ++;
  }

  if (matchAttrs) {
    for (auto& i : formals->formals) {
      newEnv.vars[i.name] = displ++;
    }

    for (auto& i : formals->formals) {
      if (i.def != nullptr) {
        i.def->bindVars(newEnv);
      }
    }
  }

  body->bindVars(newEnv);
}

void ExprLet::bindVars(const StaticEnv& env) {
  StaticEnv newEnv(false, &env);

  unsigned int displ = 0;
  for (auto& i : attrs->attrs) {
    newEnv.vars[i.first] = i.second.displ = displ++;
  }

  for (auto& i : attrs->attrs) {
    i.second.e->bindVars(i.second.inherited ? env : newEnv);
  }

  body->bindVars(newEnv);
}

void ExprWith::bindVars(const StaticEnv& env) {
  /* Does this `with' have an enclosing `with'?  If so, record its
     level so that `lookupVar' can look up variables in the previous
     `with' if this one doesn't contain the desired attribute. */
  const StaticEnv* curEnv;
  unsigned int level;
  prevWith = 0;
  for (curEnv = &env, level = 1; curEnv != nullptr;
       curEnv = curEnv->up, level++) {
    if (curEnv->isWith) {
      prevWith = level;
      break;
    }
  }

  attrs->bindVars(env);
  StaticEnv newEnv(true, &env);
  body->bindVars(newEnv);
}

void ExprIf::bindVars(const StaticEnv& env) {
  cond->bindVars(env);
  then->bindVars(env);
  else_->bindVars(env);
}

void ExprAssert::bindVars(const StaticEnv& env) {
  cond->bindVars(env);
  body->bindVars(env);
}

void ExprOpNot::bindVars(const StaticEnv& env) { e->bindVars(env); }

void ExprConcatStrings::bindVars(const StaticEnv& env) {
  for (auto& i : *es) {
    i->bindVars(env);
  }
}

void ExprPos::bindVars(const StaticEnv& env) {}

/* Storing function names. */
void ExprLambda::setName(Symbol& name) { this->name = name; }

std::string ExprLambda::showNamePos() const {
  return (format("%1% at %2%") %
          (name.has_value() ? "'" + std::string(name.value()) + "'"
                            : "anonymous function") %
          pos)
      .str();
}

}  // namespace nix
