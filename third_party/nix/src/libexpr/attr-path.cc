#include "libexpr/attr-path.hh"

#include <absl/strings/numbers.h>

#include "libexpr/eval-inline.hh"
#include "libutil/util.hh"

namespace nix {

static Strings parseAttrPath(const std::string& s) {
  Strings res;
  std::string cur;
  std::string::const_iterator i = s.begin();
  while (i != s.end()) {
    if (*i == '.') {
      res.push_back(cur);
      cur.clear();
    } else if (*i == '"') {
      ++i;
      while (true) {
        if (i == s.end()) {
          throw Error(format("missing closing quote in selection path '%1%'") %
                      s);
        }
        if (*i == '"') {
          break;
        }
        cur.push_back(*i++);
      }
    } else {
      cur.push_back(*i);
    }
    ++i;
  }
  if (!cur.empty()) {
    res.push_back(cur);
  }
  return res;
}

Value* findAlongAttrPath(EvalState& state, const std::string& attrPath,
                         Bindings& autoArgs, Value& vIn) {
  Strings tokens = parseAttrPath(attrPath);

  Error attrError =
      Error(format("attribute selection path '%1%' does not match expression") %
            attrPath);

  Value* v = &vIn;

  for (auto& attr : tokens) {
    /* Is i an index (integer) or a normal attribute name? */
    enum { apAttr, apIndex } apType = apAttr;
    unsigned int attrIndex = 0;
    if (absl::SimpleAtoi(attr, &attrIndex)) {
      apType = apIndex;
    }

    /* Evaluate the expression. */
    Value* vNew = state.allocValue();
    state.autoCallFunction(autoArgs, *v, *vNew);
    v = vNew;
    state.forceValue(*v);

    /* It should evaluate to either a set or an expression,
       according to what is specified in the attrPath. */

    if (apType == apAttr) {
      if (v->type != tAttrs) {
        throw TypeError(format("the expression selected by the selection path "
                               "'%1%' should be a set but is %2%") %
                        attrPath % showType(*v));
      }

      if (attr.empty()) {
        throw Error(format("empty attribute name in selection path '%1%'") %
                    attrPath);
      }

      Bindings::iterator a = v->attrs->find(state.symbols.Create(attr));
      if (a == v->attrs->end()) {
        throw Error(
            format("attribute '%1%' in selection path '%2%' not found") % attr %
            attrPath);
      }
      v = &*(a->second).value;
    }

    else if (apType == apIndex) {
      if (!v->isList()) {
        throw TypeError(format("the expression selected by the selection path "
                               "'%1%' should be a list but is %2%") %
                        attrPath % showType(*v));
      }

      if (attrIndex >= v->listSize()) {
        throw Error(
            format("list index %1% in selection path '%2%' is out of range") %
            attrIndex % attrPath);
      }

      v = (*v->list)[attrIndex];
    }
  }

  return v;
}

}  // namespace nix
