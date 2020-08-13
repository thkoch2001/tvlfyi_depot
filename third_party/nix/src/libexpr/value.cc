#include "libexpr/value.hh"

namespace nix {

Value::Value(const Value& copy) { *this = copy; }

Value::Value(Value&& move) { *this = move; }

Value& Value::operator=(const Value& copy) {
  if (type != copy.type) {
    memset(this, 0, sizeof(*this));
  }
  type = copy.type;
  switch (type) {
    case tInt:
      integer = copy.integer;
      break;
    case tBool:
      boolean = copy.boolean;
      break;
    case tString:
      string = copy.string;
      break;
    case tPath:
      path = copy.path;
      break;
    case tNull:
      /* no fields */
      break;
    case tAttrs:
      attrs = copy.attrs;
      break;
    case tList:
      list = copy.list;
      break;
    case tThunk:
      thunk = copy.thunk;
      break;
    case tApp:
      app = copy.app;
      break;
    case tLambda:
      lambda = copy.lambda;
      break;
    case tBlackhole:
      /* no fields */
      break;
    case tPrimOp:
      primOp = copy.primOp;
      break;
    case tPrimOpApp:
      primOpApp = copy.primOpApp;
      break;
    case _reserved1:
      assert(false);
      break;
    case tFloat:
      fpoint = copy.fpoint;
      break;
  }
  return *this;
}

Value& Value::operator=(Value&& move) {
  if (type != move.type) {
    memset(this, 0, sizeof(*this));
  }
  type = move.type;
  switch (type) {
    case tInt:
      integer = move.integer;
      break;
    case tBool:
      boolean = move.boolean;
      break;
    case tString:
      string = move.string;
      break;
    case tPath:
      path = move.path;
      break;
    case tNull:
      /* no fields */
      break;
    case tAttrs:
      attrs = move.attrs;
      break;
    case tList:
      list = move.list;
      break;
    case tThunk:
      thunk = move.thunk;
      break;
    case tApp:
      app = move.app;
      break;
    case tLambda:
      lambda = move.lambda;
      break;
    case tBlackhole:
      /* no fields */
      break;
    case tPrimOp:
      primOp = move.primOp;
      break;
    case tPrimOpApp:
      primOpApp = move.primOpApp;
      break;
    case _reserved1:
      assert(false);
      break;
    case tFloat:
      fpoint = move.fpoint;
      break;
  }
  return *this;
}

}  // namespace nix
