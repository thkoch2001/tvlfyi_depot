#pragma once

#include <tuple>
#include <vector>

#include "libexpr/symbol-table.hh"
#include "libutil/types.hh"

namespace nix {

using ValueType = enum {
  tInt = 1,
  tBool,
  tString,
  tPath,
  tNull,
  tAttrs,
  tList,
  tThunk,
  tApp,
  tLambda,
  tBlackhole,
  tPrimOp,
  tPrimOpApp,
  _reserved1,  // formerly tExternal
  tFloat
};

class Bindings;
struct Env;
struct Expr;
struct ExprLambda;
struct PrimOp;
struct PrimOp;
class Symbol;

typedef int64_t NixInt;
typedef double NixFloat;

// Forward declaration of Value is required because the following
// types are mutually recursive.
//
// TODO(tazjin): Really, these types need some serious refactoring.
struct Value;

/* Strings in the evaluator carry a so-called `context' which
   is a list of strings representing store paths.  This is to
   allow users to write things like

   "--with-freetype2-library=" + freetype + "/lib"

   where `freetype' is a derivation (or a source to be copied
   to the store).  If we just concatenated the strings without
   keeping track of the referenced store paths, then if the
   string is used as a derivation attribute, the derivation
   will not have the correct dependencies in its inputDrvs and
   inputSrcs.

   The semantics of the context is as follows: when a string
   with context C is used as a derivation attribute, then the
   derivations in C will be added to the inputDrvs of the
   derivation, and the other store paths in C will be added to
   the inputSrcs of the derivations.

   For canonicity, the store paths should be in sorted order. */
struct NixString {
  const char* s;
  const char** context;  // must be in sorted order
};

struct NixThunk {
  Env* env;
  Expr* expr;
};

struct NixApp {
  Value *left, *right;
};

struct NixLambda {
  Env* env;
  ExprLambda* fun;
};

struct NixPrimOpApp {
  Value *left, *right;
};

using NixList = std::vector<Value*>;

struct Value {
  ValueType type;
  union {  // TODO(tazjin): std::variant
    NixInt integer;
    bool boolean;
    NixString string;
    const char* path;
    std::shared_ptr<Bindings> attrs;
    std::shared_ptr<NixList> list;
    NixThunk thunk;
    NixApp app;  // TODO(tazjin): "app"?
    NixLambda lambda;
    PrimOp* primOp;
    NixPrimOpApp primOpApp;
    NixFloat fpoint;
  };

  Value() : type(tInt), attrs(nullptr) {
    static_assert(offsetof(Value, attrs) + sizeof(attrs) == sizeof(Value));
  }

  Value(const Value& copy);
  Value(Value&& move);
  ~Value() {}
  Value& operator=(const Value& copy);
  Value& operator=(Value&& move);

  bool isList() const { return type == tList; }

  size_t listSize() const { return list->size(); }
};

/* After overwriting an app node, be sure to clear pointers in the
   Value to ensure that the target isn't kept alive unnecessarily. */
static inline void clearValue(Value& v) { v.app.left = v.app.right = 0; }

static inline void mkInt(Value& v, NixInt n) {
  clearValue(v);
  v.type = tInt;
  v.integer = n;
}

static inline void mkFloat(Value& v, NixFloat n) {
  clearValue(v);
  v.type = tFloat;
  v.fpoint = n;
}

static inline void mkBool(Value& v, bool b) {
  clearValue(v);
  v.type = tBool;
  v.boolean = b;
}

static inline void mkNull(Value& v) {
  clearValue(v);
  v.type = tNull;
}

static inline void mkApp(Value& v, Value& left, Value& right) {
  v.type = tApp;
  v.app.left = &left;
  v.app.right = &right;
}

static inline void mkPrimOpApp(Value& v, Value& left, Value& right) {
  v.type = tPrimOpApp;
  v.app.left = &left;
  v.app.right = &right;
}

static inline void mkStringNoCopy(Value& v, const char* s) {
  v.type = tString;
  v.string.s = s;
  v.string.context = 0;
}

static inline void mkString(Value& v, const Symbol& s) {
  mkStringNoCopy(v, ((const std::string&)s).c_str());
}

void mkString(Value& v, const char* s);

static inline void mkPathNoCopy(Value& v, const char* s) {
  clearValue(v);
  v.type = tPath;
  v.path = s;
}

void mkPath(Value& v, const char* s);

/* Compute the size in bytes of the given value, including all values
   and environments reachable from it. Static expressions (Exprs) are
   not included. */
size_t valueSize(const Value& v);

using ValueMap = std::map<Symbol, Value*>;

std::shared_ptr<Value*> allocRootValue(Value* v);

}  // namespace nix
