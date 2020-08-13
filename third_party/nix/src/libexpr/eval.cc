#include "libexpr/eval.hh"

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <new>
#include <optional>
#include <variant>

#include <absl/base/call_once.h>
#include <absl/container/flat_hash_set.h>
#include <absl/strings/match.h>
#include <glog/logging.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <unistd.h>

#include "libexpr/eval-inline.hh"
#include "libexpr/function-trace.hh"
#include "libexpr/value.hh"
#include "libstore/derivations.hh"
#include "libstore/download.hh"
#include "libstore/globals.hh"
#include "libstore/store-api.hh"
#include "libutil/hash.hh"
#include "libutil/json.hh"
#include "libutil/util.hh"
#include "libutil/visitor.hh"

namespace nix {
namespace {

void ConfigureGc() { /* This function intentionally left blank. */
}

}  // namespace

namespace expr {

absl::once_flag gc_flag;

void InitGC() { absl::call_once(gc_flag, &ConfigureGc); }

}  // namespace expr

static char* dupString(const char* s) {
  char* t;
  t = strdup(s);
  if (t == nullptr) {
    throw std::bad_alloc();
  }
  return t;
}

std::shared_ptr<Value*> allocRootValue(Value* v) {
  return std::make_shared<Value*>(v);
}

static void printValue(std::ostream& str, std::set<const Value*>& active,
                       const Value& v) {
  checkInterrupt();

  if (active.find(&v) != active.end()) {
    str << "<CYCLE>";
    return;
  }
  active.insert(&v);

  switch (v.type) {
    case tInt:
      str << v.integer;
      break;
    case tBool:
      str << (v.boolean ? "true" : "false");
      break;
    case tString:
      str << "\"";
      for (const char* i = v.string.s; *i != 0; i++) {
        if (*i == '\"' || *i == '\\') {
          str << "\\" << *i;
        } else if (*i == '\n') {
          str << "\\n";
        } else if (*i == '\r') {
          str << "\\r";
        } else if (*i == '\t') {
          str << "\\t";
        } else {
          str << *i;
        }
      }
      str << "\"";
      break;
    case tPath:
      str << v.path;  // !!! escaping?
      break;
    case tNull:
      str << "null";
      break;
    case tAttrs: {
      str << "{ ";
      for (auto& i : v.attrs->SortedByKeys()) {
        str << i->name << " = ";
        printValue(str, active, *i->value);
        str << "; ";
      }
      str << "}";
      break;
    }
    case tList:
      str << "[ ";
      for (unsigned int n = 0; n < v.listSize(); ++n) {
        printValue(str, active, *(*v.list)[n]);
        str << " ";
      }
      str << "]";
      break;
    case tThunk:
    case tApp:
      str << "<CODE>";
      break;
    case tLambda:
      str << "<LAMBDA>";
      break;
    case tPrimOp:
      str << "<PRIMOP>";
      break;
    case tPrimOpApp:
      str << "<PRIMOP-APP>";
      break;
    case tFloat:
      str << v.fpoint;
      break;
    default:
      throw Error(
          absl::StrCat("invalid value of type ", static_cast<int>(v.type)));
  }

  active.erase(&v);
}

std::ostream& operator<<(std::ostream& str, const Value& v) {
  std::set<const Value*> active;
  printValue(str, active, v);
  return str;
}

const Value* getPrimOp(const Value& v) {
  const Value* primOp = &v;
  while (primOp->type == tPrimOpApp) {
    primOp = primOp->primOpApp.left;
  }
  assert(primOp->type == tPrimOp);
  return primOp;
}

std::string showType(const Value& v) {
  switch (v.type) {
    case tInt:
      return "an integer";
    case tBool:
      return "a boolean";
    case tString:
      return v.string.context != nullptr ? "a string with context" : "a string";
    case tPath:
      return "a path";
    case tNull:
      return "null";
    case tAttrs:
      return "a set";
    case tList:
      return "a list";
    case tThunk:
      return "a thunk";
    case tApp:
      return "a function application";
    case tLambda:
      return "a function";
    case tBlackhole:
      return "a black hole";
    case tPrimOp:
      return fmt("the built-in function '%s'", std::string(v.primOp->name));
    case tPrimOpApp:
      return fmt("the partially applied built-in function '%s'",
                 std::string(getPrimOp(v)->primOp->name));
    case _reserved1:
      LOG(FATAL) << "attempted to show the type string of the deprecated "
                    "tExternal value";
      break;
    case tFloat:
      return "a float";
  }
  LOG(FATAL)
      << "attempted to determine the type string of an unknown type number ("
      << static_cast<int>(v.type) << ")";
  abort();
}

static Symbol getName(const AttrName& name, EvalState& state, Env& env) {
  return std::visit(
      util::overloaded{[&](const Symbol& name) -> Symbol { return name; },
                       [&](Expr* expr) -> Symbol {
                         Value nameValue;
                         expr->eval(state, env, nameValue);
                         state.forceStringNoCtx(nameValue);
                         return state.symbols.Create(nameValue.string.s);
                       }},
      name);
}

/* Very hacky way to parse $NIX_PATH, which is colon-separated, but
   can contain URLs (e.g. "nixpkgs=https://bla...:foo=https://"). */
static Strings parseNixPath(const std::string& s) {
  Strings res;

  auto p = s.begin();

  while (p != s.end()) {
    auto start = p;
    auto start2 = p;

    while (p != s.end() && *p != ':') {
      if (*p == '=') {
        start2 = p + 1;
      }
      ++p;
    }

    if (p == s.end()) {
      if (p != start) {
        res.push_back(std::string(start, p));
      }
      break;
    }

    if (*p == ':') {
      if (isUri(std::string(start2, s.end()))) {
        ++p;
        while (p != s.end() && *p != ':') {
          ++p;
        }
      }
      res.push_back(std::string(start, p));
      if (p == s.end()) {
        break;
      }
    }

    ++p;
  }

  return res;
}

EvalState::EvalState(const Strings& _searchPath, const ref<Store>& store)
    : sWith(symbols.Create("<with>")),
      sOutPath(symbols.Create("outPath")),
      sDrvPath(symbols.Create("drvPath")),
      sType(symbols.Create("type")),
      sMeta(symbols.Create("meta")),
      sName(symbols.Create("name")),
      sValue(symbols.Create("value")),
      sSystem(symbols.Create("system")),
      sOutputs(symbols.Create("outputs")),
      sOutputName(symbols.Create("outputName")),
      sIgnoreNulls(symbols.Create("__ignoreNulls")),
      sFile(symbols.Create("file")),
      sLine(symbols.Create("line")),
      sColumn(symbols.Create("column")),
      sFunctor(symbols.Create("__functor")),
      sToString(symbols.Create("__toString")),
      sRight(symbols.Create("right")),
      sWrong(symbols.Create("wrong")),
      sStructuredAttrs(symbols.Create("__structuredAttrs")),
      sBuilder(symbols.Create("builder")),
      sArgs(symbols.Create("args")),
      sOutputHash(symbols.Create("outputHash")),
      sOutputHashAlgo(symbols.Create("outputHashAlgo")),
      sOutputHashMode(symbols.Create("outputHashMode")),
      sDerivationNix(std::nullopt),
      repair(NoRepair),
      store(store),
      baseEnv(allocEnv(128)),
      staticBaseEnv(false, nullptr) {
  expr::InitGC();

  countCalls = getEnv("NIX_COUNT_CALLS", "0") != "0";

  /* Initialise the Nix expression search path. */
  if (!evalSettings.pureEval) {
    Strings paths = parseNixPath(getEnv("NIX_PATH", ""));
    for (auto& i : _searchPath) {
      addToSearchPath(i);
    }
    for (auto& i : paths) {
      addToSearchPath(i);
    }
  }
  addToSearchPath("nix=" +
                  canonPath(settings.nixDataDir + "/nix/corepkgs", true));

  if (evalSettings.restrictEval || evalSettings.pureEval) {
    allowedPaths = PathSet();

    for (auto& i : searchPath) {
      auto r = resolveSearchPathElem(i);
      if (!r.first) {
        continue;
      }

      auto path = r.second;

      if (store->isInStore(r.second)) {
        PathSet closure;
        store->computeFSClosure(store->toStorePath(r.second), closure);
        for (auto& path : closure) {
          allowedPaths->insert(path);
        }
      } else {
        allowedPaths->insert(r.second);
      }
    }
  }

  createBaseEnv();
}

EvalState::~EvalState() = default;

Path EvalState::checkSourcePath(const Path& path_) {
  TraceFileAccess(path_);
  if (!allowedPaths) {
    return path_;
  }

  auto i = resolvedPaths.find(path_);
  if (i != resolvedPaths.end()) {
    return i->second;
  }

  bool found = false;

  /* First canonicalize the path without symlinks, so we make sure an
   * attacker can't append ../../... to a path that would be in allowedPaths
   * and thus leak symlink targets.
   */
  Path abspath = canonPath(path_);

  for (auto& i : *allowedPaths) {
    if (isDirOrInDir(abspath, i)) {
      found = true;
      break;
    }
  }

  if (!found) {
    throw RestrictedPathError(
        "access to path '%1%' is forbidden in restricted mode", abspath);
  }

  /* Resolve symlinks. */
  DLOG(INFO) << "checking access to '" << abspath << "'";
  Path path = canonPath(abspath, true);

  for (auto& i : *allowedPaths) {
    if (isDirOrInDir(path, i)) {
      resolvedPaths[path_] = path;
      return path;
    }
  }

  throw RestrictedPathError(
      "access to path '%1%' is forbidden in restricted mode", path);
}

void EvalState::checkURI(const std::string& uri) {
  if (!evalSettings.restrictEval) {
    return;
  }

  /* 'uri' should be equal to a prefix, or in a subdirectory of a
     prefix. Thus, the prefix https://github.co does not permit
     access to https://github.com. Note: this allows 'http://' and
     'https://' as prefixes for any http/https URI. */
  for (auto& prefix : evalSettings.allowedUris.get()) {
    if (uri == prefix ||
        (uri.size() > prefix.size() && !prefix.empty() &&
         absl::StartsWith(uri, prefix) &&
         (prefix[prefix.size() - 1] == '/' || uri[prefix.size()] == '/'))) {
      return;
    }
  }

  /* If the URI is a path, then check it against allowedPaths as
     well. */
  if (absl::StartsWith(uri, "/")) {
    checkSourcePath(uri);
    return;
  }

  if (absl::StartsWith(uri, "file://")) {
    checkSourcePath(std::string(uri, 7));
    return;
  }

  throw RestrictedPathError(
      "access to URI '%s' is forbidden in restricted mode", uri);
}

Path EvalState::toRealPath(const Path& path, const PathSet& context) {
  // FIXME: check whether 'path' is in 'context'.
  return !context.empty() && store->isInStore(path) ? store->toRealPath(path)
                                                    : path;
};

Value* EvalState::addConstant(const std::string& name, Value& v) {
  Value* v2 = allocValue();
  *v2 = v;
  staticBaseEnv.vars[symbols.Create(name)] = baseEnvDispl;
  baseEnv.values[baseEnvDispl++] = v2;
  std::string name2 =
      std::string(name, 0, 2) == "__" ? std::string(name, 2) : name;
  baseEnv.values[0]->attrs->push_back(Attr(symbols.Create(name2), v2));
  return v2;
}

Value* EvalState::addPrimOp(const std::string& name, size_t arity,
                            PrimOpFun primOp) {
  if (arity == 0) {
    Value v;
    primOp(*this, noPos, nullptr, v);
    return addConstant(name, v);
  }
  Value* v = allocValue();
  std::string name2 =
      std::string(name, 0, 2) == "__" ? std::string(name, 2) : name;
  Symbol sym = symbols.Create(name2);
  // Even though PrimOp doesn't need tracing, it needs to be collected.
  v->type = tPrimOp;
  v->primOp = new PrimOp(primOp, arity, sym);
  staticBaseEnv.vars[symbols.Create(name)] = baseEnvDispl;
  baseEnv.values[baseEnvDispl++] = v;
  baseEnv.values[0]->attrs->push_back(Attr(sym, v));
  return v;
}

Value& EvalState::getBuiltin(const std::string& name) {
  return *baseEnv.values[0]->attrs->find(symbols.Create(name))->second.value;
}

/* Every "format" object (even temporary) takes up a few hundred bytes
   of stack space, which is a real killer in the recursive
   evaluator.  So here are some helper functions for throwing
   exceptions. */

LocalNoInlineNoReturn(void throwEvalError(const char* s,
                                          const std::string& s2)) {
  throw EvalError(format(s) % s2);
}

LocalNoInlineNoReturn(void throwEvalError(const char* s, const std::string& s2,
                                          const Pos& pos)) {
  throw EvalError(format(s) % s2 % pos);
}

LocalNoInlineNoReturn(void throwEvalError(const char* s, const std::string& s2,
                                          const std::string& s3)) {
  throw EvalError(format(s) % s2 % s3);
}

LocalNoInlineNoReturn(void throwEvalError(const char* s, const std::string& s2,
                                          const std::string& s3,
                                          const Pos& pos)) {
  throw EvalError(format(s) % s2 % s3 % pos);
}

LocalNoInlineNoReturn(void throwEvalError(const char* s, const Symbol& sym,
                                          const Pos& p1, const Pos& p2)) {
  throw EvalError(format(s) % sym % p1 % p2);
}

LocalNoInlineNoReturn(void throwTypeError(const char* s, const Pos& pos)) {
  throw TypeError(format(s) % pos);
}

LocalNoInlineNoReturn(void throwTypeError(const char* s,
                                          const std::string& s1)) {
  throw TypeError(format(s) % s1);
}

LocalNoInlineNoReturn(void throwTypeError(const char* s, const ExprLambda& fun,
                                          const Symbol& s2, const Pos& pos)) {
  throw TypeError(format(s) % fun.showNamePos() % s2 % pos);
}

LocalNoInlineNoReturn(void throwAssertionError(const char* s,
                                               const std::string& s1,
                                               const Pos& pos)) {
  throw AssertionError(format(s) % s1 % pos);
}

LocalNoInlineNoReturn(void throwUndefinedVarError(const char* s,
                                                  const std::string& s1,
                                                  const Pos& pos)) {
  throw UndefinedVarError(format(s) % s1 % pos);
}

LocalNoInline(void addErrorPrefix(Error& e, const char* s,
                                  const std::string& s2)) {
  e.addPrefix(format(s) % s2);
}

LocalNoInline(void addErrorPrefix(Error& e, const char* s,
                                  const ExprLambda& fun, const Pos& pos)) {
  e.addPrefix(format(s) % fun.showNamePos() % pos);
}

LocalNoInline(void addErrorPrefix(Error& e, const char* s,
                                  const std::string& s2, const Pos& pos)) {
  e.addPrefix(format(s) % s2 % pos);
}

void mkString(Value& v, const char* s) { mkStringNoCopy(v, dupString(s)); }

Value& mkString(Value& v, const std::string& s, const PathSet& context) {
  mkString(v, s.c_str());
  if (!context.empty()) {
    size_t n = 0;
    v.string.context = static_cast<const char**>(
        allocBytes((context.size() + 1) * sizeof(char*)));
    for (auto& i : context) {
      v.string.context[n++] = dupString(i.c_str());
    }
    v.string.context[n] = nullptr;
  }
  return v;
}

void mkPath(Value& v, const char* s) { mkPathNoCopy(v, dupString(s)); }

inline Value* EvalState::lookupVar(Env* env, const ExprVar& var, bool noEval) {
  for (size_t l = var.level; l != 0u; --l, env = env->up) {
    ;
  }

  if (!var.fromWith) {
    return env->values[var.displ];
  }

  while (true) {
    if (env->type == Env::HasWithExpr) {
      if (noEval) {
        return nullptr;
      }
      if (!env->withAttrsExpr) {
        CHECK(false) << "HasWithExpr evaluated twice";
      }
      Value* v = allocValue();
      evalAttrs(*env->up, env->withAttrsExpr, *v);
      env->values[0] = v;
      env->withAttrsExpr = nullptr;
      env->type = Env::HasWithAttrs;
    }
    Bindings::iterator j = env->values[0]->attrs->find(var.name);
    if (j != env->values[0]->attrs->end()) {
      if (countCalls && (j->second.pos != nullptr)) {
        attrSelects[*j->second.pos]++;
      }
      return j->second.value;
    }
    if (env->prevWith == 0u) {
      throwUndefinedVarError("undefined variable '%1%' at %2%", var.name,
                             var.pos);
    }
    for (size_t l = env->prevWith; l != 0u; --l, env = env->up) {
    }
  }
}

Value* EvalState::allocValue() {
  nrValues++;
  return new Value;
}

Env& EvalState::allocEnv(size_t size) {
  if (size > std::numeric_limits<decltype(Env::size)>::max()) {
    throw Error("environment size %d is too big", size);
  }

  nrEnvs++;
  nrValuesInEnvs += size;
  Env* env = new Env(size);
  env->type = Env::Plain;

  return *env;
}

void EvalState::mkList(Value& v, NixList* list) {
  clearValue(v);
  v.type = tList;
  v.list = list;
  nrListElems += list->size();
}

void EvalState::mkList(Value& v, size_t size) {
  EvalState::mkList(v, new NixList(size));
}

unsigned long nrThunks = 0;

static inline void mkThunk(Value& v, Env& env, Expr* expr) {
  v.type = tThunk;
  v.thunk.env = &env;
  v.thunk.expr = expr;
  nrThunks++;
}

void EvalState::mkThunk_(Value& v, Expr* expr) { mkThunk(v, baseEnv, expr); }

void EvalState::mkPos(Value& v, Pos* pos) {
  if ((pos != nullptr) && pos->file.has_value() && pos->file.value().set()) {
    mkAttrs(v, 3);
    mkString(*allocAttr(v, sFile), pos->file.value());
    mkInt(*allocAttr(v, sLine), pos->line);
    mkInt(*allocAttr(v, sColumn), pos->column);
  } else {
    mkNull(v);
  }
}

/* Create a thunk for the delayed computation of the given expression
   in the given environment.  But if the expression is a variable,
   then look it up right away.  This significantly reduces the number
   of thunks allocated. */
Value* Expr::maybeThunk(EvalState& state, Env& env) {
  Value* v = state.allocValue();
  mkThunk(*v, env, this);
  return v;
}

unsigned long nrAvoided = 0;

Value* ExprVar::maybeThunk(EvalState& state, Env& env) {
  Value* v = state.lookupVar(&env, *this, true);
  /* The value might not be initialised in the environment yet.
     In that case, ignore it. */
  if (v != nullptr) {
    nrAvoided++;
    return v;
  }
  return Expr::maybeThunk(state, env);
}

Value* ExprString::maybeThunk(EvalState& state, Env& env) {
  nrAvoided++;
  return &v;
}

Value* ExprInt::maybeThunk(EvalState& state, Env& env) {
  nrAvoided++;
  return &v;
}

Value* ExprFloat::maybeThunk(EvalState& state, Env& env) {
  nrAvoided++;
  return &v;
}

Value* ExprPath::maybeThunk(EvalState& state, Env& env) {
  nrAvoided++;
  return &v;
}

void EvalState::evalFile(const Path& path_, Value& v) {
  auto path = checkSourcePath(path_);

  FileEvalCache::iterator i;
  if ((i = fileEvalCache.find(path)) != fileEvalCache.end()) {
    v = i->second;
    return;
  }

  Path path2 = resolveExprPath(path);
  if ((i = fileEvalCache.find(path2)) != fileEvalCache.end()) {
    v = i->second;
    return;
  }

  DLOG(INFO) << "evaluating file '" << path2 << "'";
  Expr* e = nullptr;

  auto j = fileParseCache.find(path2);
  if (j != fileParseCache.end()) {
    e = j->second;
  }

  if (e == nullptr) {
    e = parseExprFromFile(checkSourcePath(path2));
  }

  fileParseCache[path2] = e;

  try {
    eval(e, v);
  } catch (Error& e) {
    addErrorPrefix(e, "while evaluating the file '%1%':\n", path2);
    throw;
  }

  fileEvalCache[path2] = v;
  if (path != path2) {
    fileEvalCache[path] = v;
  }
}

void EvalState::resetFileCache() {
  fileEvalCache.clear();
  fileParseCache.clear();
}

void EvalState::eval(Expr* e, Value& v) { e->eval(*this, baseEnv, v); }

inline bool EvalState::evalBool(Env& env, Expr* e) {
  Value v;
  e->eval(*this, env, v);
  if (v.type != tBool) {
    throwTypeError("value is %1% while a Boolean was expected", v);
  }
  return v.boolean;
}

inline bool EvalState::evalBool(Env& env, Expr* e, const Pos& pos) {
  Value v;
  e->eval(*this, env, v);
  if (v.type != tBool) {
    throwTypeError("value is %1% while a Boolean was expected, at %2%", v, pos);
  }
  return v.boolean;
}

inline void EvalState::evalAttrs(Env& env, Expr* e, Value& v) {
  e->eval(*this, env, v);
  if (v.type != tAttrs) {
    throwTypeError("value is %1% while a set was expected", v);
  }
}

void Expr::eval(EvalState& state, Env& env, Value& v) { abort(); }

void ExprInt::eval(EvalState& state, Env& env, Value& v) { v = this->v; }

void ExprFloat::eval(EvalState& state, Env& env, Value& v) { v = this->v; }

void ExprString::eval(EvalState& state, Env& env, Value& v) { v = this->v; }

void ExprPath::eval(EvalState& state, Env& env, Value& v) { v = this->v; }

void ExprAttrs::eval(EvalState& state, Env& env, Value& value) {
  state.mkAttrs(value, attrs.size() + dynamicAttrs.size());
  Env* dynamicEnv = &env;

  if (recursive) {
    /* Create a new environment that contains the attributes in
       this `rec'. */
    Env& env2(state.allocEnv(attrs.size()));
    env2.up = &env;
    dynamicEnv = &env2;

    /* The recursive attributes are evaluated in the new
       environment, while the inherited attributes are evaluated
       in the original environment. */
    size_t displ = 0;
    for (auto& attr : attrs) {
      Value* vAttr;
      vAttr =
          attr.second.e->maybeThunk(state, attr.second.inherited ? env : env2);
      env2.values[displ++] = vAttr;
      value.attrs->push_back(Attr(attr.first, vAttr, &attr.second.pos));
    }
  } else {
    // TODO(tazjin): insert range
    for (auto& i : attrs) {
      value.attrs->push_back(
          Attr(i.first, i.second.e->maybeThunk(state, env), &i.second.pos));
    }
  }

  /* Dynamic attrs apply *after* rec. */
  for (auto& i : dynamicAttrs) {
    Value nameVal;
    i.nameExpr->eval(state, *dynamicEnv, nameVal);
    state.forceValue(nameVal, i.pos);
    if (nameVal.type == tNull) {
      continue;
    }
    state.forceStringNoCtx(nameVal);
    Symbol nameSym = state.symbols.Create(nameVal.string.s);
    Bindings::iterator j = value.attrs->find(nameSym);
    if (j != value.attrs->end()) {
      throwEvalError("dynamic attribute '%1%' at %2% already defined at %3%",
                     nameSym, i.pos, *j->second.pos);
    }

    value.attrs->push_back(
        Attr(nameSym, i.valueExpr->maybeThunk(state, *dynamicEnv), &i.pos));
  }
}

void ExprLet::eval(EvalState& state, Env& env, Value& v) {
  /* Create a new environment that contains the attributes in this
     `let'. */
  Env& env2(state.allocEnv(attrs->attrs.size()));
  env2.up = &env;

  /* The recursive attributes are evaluated in the new environment,
     while the inherited attributes are evaluated in the original
     environment. */
  size_t displ = 0;
  for (auto& i : attrs->attrs) {
    env2.values[displ++] =
        i.second.e->maybeThunk(state, i.second.inherited ? env : env2);
  }

  body->eval(state, env2, v);
}

void ExprList::eval(EvalState& state, Env& env, Value& v) {
  state.mkList(v, elems.size());
  for (size_t n = 0; n < elems.size(); ++n) {
    (*v.list)[n] = elems[n]->maybeThunk(state, env);
  }
}

void ExprVar::eval(EvalState& state, Env& env, Value& v) {
  Value* v2 = state.lookupVar(&env, *this, false);
  state.forceValue(*v2, pos);
  v = *v2;
}

static std::string showAttrPath(EvalState& state, Env& env,
                                const AttrPath& attrPath) {
  std::ostringstream out;
  bool first = true;
  for (auto& i : attrPath) {
    if (!first) {
      out << '.';
    } else {
      first = false;
    }
    out << getName(i, state, env);
  }
  return out.str();
}

uint64_t nrLookups = 0;

void ExprSelect::eval(EvalState& state, Env& env, Value& v) {
  Value vTmp;
  Pos* pos2 = nullptr;
  Value* vAttrs = &vTmp;

  e->eval(state, env, vTmp);

  try {
    for (auto& i : attrPath) {
      nrLookups++;
      Bindings::iterator j;
      Symbol name = getName(i, state, env);
      if (def != nullptr) {
        state.forceValue(*vAttrs, pos);
        if (vAttrs->type != tAttrs ||
            (j = vAttrs->attrs->find(name)) == vAttrs->attrs->end()) {
          def->eval(state, env, v);
          return;
        }
      } else {
        state.forceAttrs(*vAttrs, pos);
        if ((j = vAttrs->attrs->find(name)) == vAttrs->attrs->end()) {
          throwEvalError("attribute '%1%' missing, at %2%", name, pos);
        }
      }
      vAttrs = j->second.value;
      pos2 = j->second.pos;
      if (state.countCalls && (pos2 != nullptr)) {
        state.attrSelects[*pos2]++;
      }
    }

    state.forceValue(*vAttrs, (pos2 != nullptr ? *pos2 : this->pos));

  } catch (Error& e) {
    // This code relies on 'sDerivationNix' being correcty mutated at
    // some prior point (it would previously otherwise have been a
    // nullptr).
    //
    // We haven't seen this fail, so for now the contained value is
    // just accessed at the risk of potentially crashing.
    if ((pos2 != nullptr) && pos2->file != state.sDerivationNix.value()) {
      addErrorPrefix(e, "while evaluating the attribute '%1%' at %2%:\n",
                     showAttrPath(state, env, attrPath), *pos2);
    }
    throw;
  }

  v = *vAttrs;
}

void ExprOpHasAttr::eval(EvalState& state, Env& env, Value& v) {
  Value vTmp;
  Value* vAttrs = &vTmp;

  e->eval(state, env, vTmp);

  for (auto& i : attrPath) {
    state.forceValue(*vAttrs);
    Bindings::iterator j;
    Symbol name = getName(i, state, env);
    if (vAttrs->type != tAttrs ||
        (j = vAttrs->attrs->find(name)) == vAttrs->attrs->end()) {
      mkBool(v, false);
      return;
    }
    vAttrs = j->second.value;
  }

  mkBool(v, true);
}

void ExprLambda::eval(EvalState& state, Env& env, Value& v) {
  v.type = tLambda;
  v.lambda.env = &env;
  v.lambda.fun = this;
}

void ExprApp::eval(EvalState& state, Env& env, Value& v) {
  /* FIXME: vFun prevents GCC from doing tail call optimisation. */
  Value vFun;
  e1->eval(state, env, vFun);
  state.callFunction(vFun, *(e2->maybeThunk(state, env)), v, pos);
}

void EvalState::callPrimOp(Value& fun, Value& arg, Value& v, const Pos& pos) {
  /* Figure out the number of arguments still needed. */
  size_t argsDone = 0;
  Value* primOp = &fun;
  while (primOp->type == tPrimOpApp) {
    argsDone++;
    primOp = primOp->primOpApp.left;
  }
  assert(primOp->type == tPrimOp);
  auto arity = primOp->primOp->arity;
  auto argsLeft = arity - argsDone;

  if (argsLeft == 1) {
    /* We have all the arguments, so call the primop. */

    /* Put all the arguments in an array. */
    Value* vArgs[arity];
    auto n = arity - 1;
    vArgs[n--] = &arg;
    for (Value* arg = &fun; arg->type == tPrimOpApp;
         arg = arg->primOpApp.left) {
      vArgs[n--] = arg->primOpApp.right;
    }

    /* And call the primop. */
    nrPrimOpCalls++;
    if (countCalls) {
      primOpCalls[primOp->primOp->name]++;
    }
    primOp->primOp->fun(*this, pos, vArgs, v);
  } else {
    Value* fun2 = allocValue();
    *fun2 = fun;
    v.type = tPrimOpApp;
    v.primOpApp.left = fun2;
    v.primOpApp.right = &arg;
  }
}

void EvalState::callFunction(Value& fun, Value& arg, Value& v, const Pos& pos) {
  auto trace = evalSettings.traceFunctionCalls
                   ? std::make_unique<FunctionCallTrace>(pos)
                   : nullptr;

  forceValue(fun, pos);

  if (fun.type == tPrimOp || fun.type == tPrimOpApp) {
    callPrimOp(fun, arg, v, pos);
    return;
  }

  // If the value to be called is an attribute set, check whether it
  // contains an appropriate function in the '__functor' element and
  // use that.
  if (fun.type == tAttrs) {
    auto found = fun.attrs->find(sFunctor);
    if (found != fun.attrs->end()) {
      // fun may be allocated on the stack of the calling function,
      // but for functors we may keep a reference, so heap-allocate a
      // copy and use that instead
      auto& fun2 = *allocValue();
      fun2 = fun;
      /* !!! Should we use the attr pos here? */
      Value v2;
      // functors are called with the element itself as the first
      // parameter, which is partially applied here
      callFunction(*found->second.value, fun2, v2, pos);
      return callFunction(v2, arg, v, pos);
    }
  }

  if (fun.type != tLambda) {
    throwTypeError(
        "attempt to call something which is not a function but %1%, at %2%",
        fun, pos);
  }

  ExprLambda& lambda(*fun.lambda.fun);

  auto size = (lambda.arg.empty() ? 0 : 1) +
              (lambda.matchAttrs ? lambda.formals->formals.size() : 0);
  Env& env2(allocEnv(size));
  env2.up = fun.lambda.env;

  size_t displ = 0;

  if (!lambda.matchAttrs) {
    env2.values[displ++] = &arg;

  } else {
    forceAttrs(arg, pos);

    if (!lambda.arg.empty()) {
      env2.values[displ++] = &arg;
    }

    /* For each formal argument, get the actual argument.  If
       there is no matching actual argument but the formal
       argument has a default, use the default. */
    size_t attrsUsed = 0;
    for (auto& i : lambda.formals->formals) {
      Bindings::iterator j = arg.attrs->find(i.name);
      if (j == arg.attrs->end()) {
        if (i.def == nullptr) {
          throwTypeError("%1% called without required argument '%2%', at %3%",
                         lambda, i.name, pos);
        }
        env2.values[displ++] = i.def->maybeThunk(*this, env2);
      } else {
        attrsUsed++;
        env2.values[displ++] = j->second.value;
      }
    }

    /* Check that each actual argument is listed as a formal
       argument (unless the attribute match specifies a `...'). */
    if (!lambda.formals->ellipsis && attrsUsed != arg.attrs->size()) {
      /* Nope, so show the first unexpected argument to the
         user. */
      for (auto& i : *arg.attrs) {
        if (lambda.formals->argNames.find(i.second.name) ==
            lambda.formals->argNames.end()) {
          throwTypeError("%1% called with unexpected argument '%2%', at %3%",
                         lambda, i.second.name, pos);
        }
      }
      abort();  // shouldn't happen
    }
  }

  nrFunctionCalls++;
  if (countCalls) {
    incrFunctionCall(&lambda);
  }

  /* Evaluate the body.  This is conditional on showTrace, because
     catching exceptions makes this function not tail-recursive. */
  if (settings.showTrace) {
    try {
      lambda.body->eval(*this, env2, v);
    } catch (Error& e) {
      addErrorPrefix(e, "while evaluating %1%, called from %2%:\n", lambda,
                     pos);
      throw;
    }
  } else {
    fun.lambda.fun->body->eval(*this, env2, v);
  }
}

// Lifted out of callFunction() because it creates a temporary that
// prevents tail-call optimisation.
void EvalState::incrFunctionCall(ExprLambda* fun) { functionCalls[fun]++; }

void EvalState::autoCallFunction(Bindings* args, Value& fun, Value& res) {
  forceValue(fun);

  if (fun.type == tAttrs) {
    auto found = fun.attrs->find(sFunctor);
    if (found != fun.attrs->end()) {
      Value* v = allocValue();
      callFunction(*found->second.value, fun, *v, noPos);
      forceValue(*v);
      return autoCallFunction(args, *v, res);
    }
  }

  if (fun.type != tLambda || !fun.lambda.fun->matchAttrs) {
    res = fun;
    return;
  }

  Value* actualArgs = allocValue();
  mkAttrs(*actualArgs, fun.lambda.fun->formals->formals.size());

  for (auto& i : fun.lambda.fun->formals->formals) {
    Bindings::iterator j = args->find(i.name);
    if (j != args->end()) {
      actualArgs->attrs->push_back(j->second);
    } else if (i.def == nullptr) {
      throwTypeError(
          "cannot auto-call a function that has an argument without a default "
          "value ('%1%')",
          i.name);
    }
  }

  callFunction(fun, *actualArgs, res, noPos);
}

void ExprWith::eval(EvalState& state, Env& env, Value& v) {
  Env& env2(state.allocEnv(1));
  env2.up = &env;
  env2.prevWith = prevWith;
  env2.type = Env::HasWithExpr;
  /* placeholder for result of attrs */
  env2.values[0] = nullptr;
  env2.withAttrsExpr = this->attrs;

  body->eval(state, env2, v);
}

void ExprIf::eval(EvalState& state, Env& env, Value& v) {
  (state.evalBool(env, cond) ? then : else_)->eval(state, env, v);
}

void ExprAssert::eval(EvalState& state, Env& env, Value& v) {
  if (!state.evalBool(env, cond, pos)) {
    std::ostringstream out;
    cond->show(out);
    throwAssertionError("assertion %1% failed at %2%", out.str(), pos);
  }
  body->eval(state, env, v);
}

void ExprOpNot::eval(EvalState& state, Env& env, Value& v) {
  mkBool(v, !state.evalBool(env, e));
}

void ExprOpEq::eval(EvalState& state, Env& env, Value& v) {
  Value v1;
  e1->eval(state, env, v1);
  Value v2;
  e2->eval(state, env, v2);
  mkBool(v, state.eqValues(v1, v2));
}

void ExprOpNEq::eval(EvalState& state, Env& env, Value& v) {
  Value v1;
  e1->eval(state, env, v1);
  Value v2;
  e2->eval(state, env, v2);
  mkBool(v, !state.eqValues(v1, v2));
}

void ExprOpAnd::eval(EvalState& state, Env& env, Value& v) {
  mkBool(v, state.evalBool(env, e1, pos) && state.evalBool(env, e2, pos));
}

void ExprOpOr::eval(EvalState& state, Env& env, Value& v) {
  mkBool(v, state.evalBool(env, e1, pos) || state.evalBool(env, e2, pos));
}

void ExprOpImpl::eval(EvalState& state, Env& env, Value& v) {
  mkBool(v, !state.evalBool(env, e1, pos) || state.evalBool(env, e2, pos));
}

void ExprOpUpdate::eval(EvalState& state, Env& env, Value& dest) {
  Value v1;
  Value v2;
  state.evalAttrs(env, e1, v1);
  state.evalAttrs(env, e2, v2);

  state.nrOpUpdates++;

  clearValue(dest);
  dest.type = tAttrs;
  dest.attrs = Bindings::Merge(*v1.attrs, *v2.attrs);
}

void ExprOpConcatLists::eval(EvalState& state, Env& env, Value& v) {
  Value v1;
  e1->eval(state, env, v1);
  Value v2;
  e2->eval(state, env, v2);
  state.concatLists(v, {&v1, &v2}, pos);
}

void EvalState::concatLists(Value& v, const NixList& lists, const Pos& pos) {
  nrListConcats++;

  auto outlist = new NixList();

  for (Value* list : lists) {
    forceList(*list, pos);
    outlist->insert(outlist->end(), list->list->begin(), list->list->end());
  }

  mkList(v, outlist);
}

void ExprConcatStrings::eval(EvalState& state, Env& env, Value& v) {
  PathSet context;
  std::ostringstream s;
  NixInt n = 0;
  NixFloat nf = 0;

  bool first = !forceString;
  ValueType firstType = tString;

  for (auto& i : *es) {
    Value vTmp;
    i->eval(state, env, vTmp);

    /* If the first element is a path, then the result will also
       be a path, we don't copy anything (yet - that's done later,
       since paths are copied when they are used in a derivation),
       and none of the strings are allowed to have contexts. */
    if (first) {
      firstType = vTmp.type;
      first = false;
    }

    if (firstType == tInt) {
      if (vTmp.type == tInt) {
        n += vTmp.integer;
      } else if (vTmp.type == tFloat) {
        // Upgrade the type from int to float;
        firstType = tFloat;
        nf = n;
        nf += vTmp.fpoint;
      } else {
        throwEvalError("cannot add %1% to an integer, at %2%", showType(vTmp),
                       pos);
      }
    } else if (firstType == tFloat) {
      if (vTmp.type == tInt) {
        nf += vTmp.integer;
      } else if (vTmp.type == tFloat) {
        nf += vTmp.fpoint;
      } else {
        throwEvalError("cannot add %1% to a float, at %2%", showType(vTmp),
                       pos);
      }
    } else {
      s << state.coerceToString(pos, vTmp, context, false,
                                firstType == tString);
    }
  }

  if (firstType == tInt) {
    mkInt(v, n);
  } else if (firstType == tFloat) {
    mkFloat(v, nf);
  } else if (firstType == tPath) {
    if (!context.empty()) {
      throwEvalError(
          "a string that refers to a store path cannot be appended to a path, "
          "at %1%",
          pos);
    }
    auto path = canonPath(s.str());
    mkPath(v, path.c_str());
  } else {
    mkString(v, s.str(), context);
  }
}

void ExprPos::eval(EvalState& state, Env& env, Value& v) {
  state.mkPos(v, &pos);
}

template <typename T>
using traceable_flat_hash_set = absl::flat_hash_set<T>;

void EvalState::forceValueDeep(Value& v) {
  traceable_flat_hash_set<const Value*> seen;

  std::function<void(Value & v)> recurse;

  recurse = [&](Value& v) {
    if (seen.find(&v) != seen.end()) {
      return;
    }
    seen.insert(&v);

    forceValue(v);

    if (v.type == tAttrs) {
      for (auto& i : *v.attrs) {
        try {
          recurse(*i.second.value);
        } catch (Error& e) {
          addErrorPrefix(e, "while evaluating the attribute '%1%' at %2%:\n",
                         i.second.name, *i.second.pos);
          throw;
        }
      }
    } else if (v.isList()) {
      for (size_t n = 0; n < v.listSize(); ++n) {
        recurse(*(*v.list)[n]);
      }
    }
  };

  recurse(v);
}

NixInt EvalState::forceInt(Value& v, const Pos& pos) {
  forceValue(v, pos);
  if (v.type != tInt) {
    throwTypeError("value is %1% while an integer was expected, at %2%", v,
                   pos);
  }
  return v.integer;
}

NixFloat EvalState::forceFloat(Value& v, const Pos& pos) {
  forceValue(v, pos);
  if (v.type == tInt) {
    return static_cast<NixFloat>(v.integer);
  }
  if (v.type != tFloat) {
    throwTypeError("value is %1% while a float was expected, at %2%", v, pos);
  }
  return v.fpoint;
}

bool EvalState::forceBool(Value& v, const Pos& pos) {
  forceValue(v);
  if (v.type != tBool) {
    throwTypeError("value is %1% while a Boolean was expected, at %2%", v, pos);
  }
  return v.boolean;
}

bool EvalState::isFunctor(Value& fun) {
  return fun.type == tAttrs && fun.attrs->find(sFunctor) != fun.attrs->end();
}

void EvalState::forceFunction(Value& v, const Pos& pos) {
  forceValue(v);
  if (v.type != tLambda && v.type != tPrimOp && v.type != tPrimOpApp &&
      !isFunctor(v)) {
    throwTypeError("value is %1% while a function was expected, at %2%", v,
                   pos);
  }
}

std::string EvalState::forceString(Value& v, const Pos& pos) {
  forceValue(v, pos);
  if (v.type != tString) {
    if (pos) {
      throwTypeError("value is %1% while a string was expected, at %2%", v,
                     pos);
    } else {
      throwTypeError("value is %1% while a string was expected", v);
    }
  }
  return std::string(v.string.s);
}

void copyContext(const Value& v, PathSet& context) {
  if (v.string.context != nullptr) {
    for (const char** p = v.string.context; *p != nullptr; ++p) {
      context.insert(*p);
    }
  }
}

std::string EvalState::forceString(Value& v, PathSet& context, const Pos& pos) {
  std::string s = forceString(v, pos);
  copyContext(v, context);
  return s;
}

std::string EvalState::forceStringNoCtx(Value& v, const Pos& pos) {
  std::string s = forceString(v, pos);
  if (v.string.context != nullptr) {
    if (pos) {
      throwEvalError(
          "the string '%1%' is not allowed to refer to a store path (such as "
          "'%2%'), at %3%",
          v.string.s, v.string.context[0], pos);
    } else {
      throwEvalError(
          "the string '%1%' is not allowed to refer to a store path (such as "
          "'%2%')",
          v.string.s, v.string.context[0]);
    }
  }
  return s;
}

bool EvalState::isDerivation(Value& v) {
  if (v.type != tAttrs) {
    return false;
  }
  Bindings::iterator i = v.attrs->find(sType);
  if (i == v.attrs->end()) {
    return false;
  }
  forceValue(*i->second.value);
  if (i->second.value->type != tString) {
    return false;
  }
  return strcmp(i->second.value->string.s, "derivation") == 0;
}

std::optional<std::string> EvalState::tryAttrsToString(const Pos& pos, Value& v,
                                                       PathSet& context,
                                                       bool coerceMore,
                                                       bool copyToStore) {
  auto i = v.attrs->find(sToString);
  if (i != v.attrs->end()) {
    Value v1;
    callFunction(*i->second.value, v, v1, pos);
    return coerceToString(pos, v1, context, coerceMore, copyToStore);
  }

  return {};
}

std::string EvalState::coerceToString(const Pos& pos, Value& v,
                                      PathSet& context, bool coerceMore,
                                      bool copyToStore) {
  forceValue(v);

  std::string s;

  if (v.type == tString) {
    copyContext(v, context);
    return v.string.s;
  }

  if (v.type == tPath) {
    Path path(canonPath(v.path));
    return copyToStore ? copyPathToStore(context, path) : path;
  }

  if (v.type == tAttrs) {
    auto maybeString =
        tryAttrsToString(pos, v, context, coerceMore, copyToStore);
    if (maybeString) {
      return *maybeString;
    }
    auto i = v.attrs->find(sOutPath);
    if (i == v.attrs->end()) {
      throwTypeError("cannot coerce a set to a string, at %1%", pos);
    }
    return coerceToString(pos, *i->second.value, context, coerceMore,
                          copyToStore);
  }

  if (coerceMore) {
    /* Note that `false' is represented as an empty string for
       shell scripting convenience, just like `null'. */
    if (v.type == tBool && v.boolean) {
      return "1";
    }
    if (v.type == tBool && !v.boolean) {
      return "";
    }
    if (v.type == tInt) {
      return std::to_string(v.integer);
    }
    if (v.type == tFloat) {
      return std::to_string(v.fpoint);
    }
    if (v.type == tNull) {
      return "";
    }

    if (v.isList()) {
      std::string result;
      for (size_t n = 0; n < v.listSize(); ++n) {
        result += coerceToString(pos, *(*v.list)[n], context, coerceMore,
                                 copyToStore);
        if (n < v.listSize() - 1
            /* !!! not quite correct */
            && (!(*v.list)[n]->isList() || (*v.list)[n]->listSize() != 0)) {
          result += " ";
        }
      }
      return result;
    }
  }

  throwTypeError("cannot coerce %1% to a string, at %2%", v, pos);
}

std::string EvalState::copyPathToStore(PathSet& context, const Path& path) {
  if (nix::isDerivation(path)) {
    throwEvalError("file names are not allowed to end in '%1%'", drvExtension);
  }

  Path dstPath;
  if (!srcToStore[path].empty()) {
    dstPath = srcToStore[path];
  } else {
    dstPath =
        settings.readOnlyMode
            ? store
                  ->computeStorePathForPath(baseNameOf(path),
                                            checkSourcePath(path))
                  .first
            : store->addToStore(baseNameOf(path), checkSourcePath(path), true,
                                htSHA256, defaultPathFilter, repair);
    srcToStore[path] = dstPath;
    DLOG(INFO) << "copied source '" << path << "' -> '" << dstPath << "'";
  }

  context.insert(dstPath);
  return dstPath;
}

Path EvalState::coerceToPath(const Pos& pos, Value& v, PathSet& context) {
  std::string path = coerceToString(pos, v, context, false, false);
  if (path.empty() || path[0] != '/') {
    throwEvalError("string '%1%' doesn't represent an absolute path, at %2%",
                   path, pos);
  }
  return path;
}

bool EvalState::eqValues(Value& v1, Value& v2) {
  forceValue(v1);
  forceValue(v2);

  /* !!! Hack to support some old broken code that relies on pointer
     equality tests between sets.  (Specifically, builderDefs calls
     uniqList on a list of sets.)  Will remove this eventually. */
  if (&v1 == &v2) {
    return true;
  }

  // Special case type-compatibility between float and int
  if (v1.type == tInt && v2.type == tFloat) {
    return v1.integer == v2.fpoint;
  }
  if (v1.type == tFloat && v2.type == tInt) {
    return v1.fpoint == v2.integer;
  }

  // All other types are not compatible with each other.
  if (v1.type != v2.type) {
    return false;
  }

  switch (v1.type) {
    case tInt:
      return v1.integer == v2.integer;

    case tBool:
      return v1.boolean == v2.boolean;

    case tString:
      return strcmp(v1.string.s, v2.string.s) == 0;

    case tPath:
      return strcmp(v1.path, v2.path) == 0;

    case tNull:
      return true;

    case tList:
      if (v1.listSize() != v2.listSize()) {
        return false;
      }
      for (size_t n = 0; n < v1.listSize(); ++n) {
        if (!eqValues(*(*v1.list)[n], *(*v2.list)[n])) {
          return false;
        }
      }
      return true;

    case tAttrs: {
      // As an optimisation if both values are pointing towards the
      // same attribute set, we can skip all this extra work.
      if (v1.attrs == v2.attrs) {
        return true;
      }

      /* If both sets denote a derivation (type = "derivation"),
         then compare their outPaths. */
      if (isDerivation(v1) && isDerivation(v2)) {
        Bindings::iterator i = v1.attrs->find(sOutPath);
        Bindings::iterator j = v2.attrs->find(sOutPath);
        if (i != v1.attrs->end() && j != v2.attrs->end()) {
          return eqValues(*i->second.value, *j->second.value);
        }
      }

      return v1.attrs->Equal(*this, v2.attrs.get());
    }

    /* Functions are incomparable. */
    case tLambda:
    case tPrimOp:
    case tPrimOpApp:
      return false;

    case tFloat:
      return v1.fpoint == v2.fpoint;

    default:
      throwEvalError("cannot compare %1% with %2%", showType(v1), showType(v2));
  }
}

void EvalState::printStats() {
  bool showStats = getEnv("NIX_SHOW_STATS", "0") != "0";

  struct rusage buf;
  getrusage(RUSAGE_SELF, &buf);
  float cpuTime = buf.ru_utime.tv_sec +
                  (static_cast<float>(buf.ru_utime.tv_usec) / 1000000);

  uint64_t bEnvs = nrEnvs * sizeof(Env) + nrValuesInEnvs * sizeof(Value*);
  uint64_t bLists = nrListElems * sizeof(Value*);
  uint64_t bValues = nrValues * sizeof(Value);
  uint64_t bAttrsets =
      nrAttrsets * sizeof(Bindings) + nrAttrsInAttrsets * sizeof(Attr);

  if (showStats) {
    auto outPath = getEnv("NIX_SHOW_STATS_PATH", "-");
    std::fstream fs;
    if (outPath != "-") {
      fs.open(outPath, std::fstream::out);
    }
    JSONObject topObj(outPath == "-" ? std::cerr : fs, true);
    topObj.attr("cpuTime", cpuTime);
    {
      auto envs = topObj.object("envs");
      envs.attr("number", nrEnvs);
      envs.attr("elements", nrValuesInEnvs);
      envs.attr("bytes", bEnvs);
    }
    {
      auto lists = topObj.object("list");
      lists.attr("elements", nrListElems);
      lists.attr("bytes", bLists);
      lists.attr("concats", nrListConcats);
    }
    {
      auto values = topObj.object("values");
      values.attr("number", nrValues);
      values.attr("bytes", bValues);
    }
    {
      auto syms = topObj.object("symbols");
      syms.attr("number", symbols.Size());
      syms.attr("bytes", symbols.TotalSize());
    }
    {
      auto sets = topObj.object("sets");
      sets.attr("number", nrAttrsets);
      sets.attr("bytes", bAttrsets);
      sets.attr("elements", nrAttrsInAttrsets);
    }
    {
      auto sizes = topObj.object("sizes");
      sizes.attr("Env", sizeof(Env));
      sizes.attr("Value", sizeof(Value));
      sizes.attr("Bindings", sizeof(Bindings));
      sizes.attr("Attr", sizeof(Attr));
    }
    topObj.attr("nrOpUpdates", nrOpUpdates);
    topObj.attr("nrOpUpdateValuesCopied", nrOpUpdateValuesCopied);
    topObj.attr("nrThunks", nrThunks);
    topObj.attr("nrAvoided", nrAvoided);
    topObj.attr("nrLookups", nrLookups);
    topObj.attr("nrPrimOpCalls", nrPrimOpCalls);
    topObj.attr("nrFunctionCalls", nrFunctionCalls);

    if (countCalls) {
      {
        auto obj = topObj.object("primops");
        for (auto& i : primOpCalls) {
          obj.attr(i.first, i.second);
        }
      }
      {
        auto list = topObj.list("functions");
        for (auto& i : functionCalls) {
          auto obj = list.object();
          if (i.first->name.has_value()) {
            obj.attr("name", (const std::string&)i.first->name.value());
          } else {
            obj.attr("name", nullptr);
          }
          if (i.first->pos) {
            obj.attr("file", (const std::string&)i.first->pos.file);
            obj.attr("line", i.first->pos.line);
            obj.attr("column", i.first->pos.column);
          }
          obj.attr("count", i.second);
        }
      }
      {
        auto list = topObj.list("attributes");
        for (auto& i : attrSelects) {
          auto obj = list.object();
          if (i.first) {
            obj.attr("file", (const std::string&)i.first.file);
            obj.attr("line", i.first.line);
            obj.attr("column", i.first.column);
          }
          obj.attr("count", i.second);
        }
      }
    }

    // TODO(tazjin): what is this? commented out because .dump() is gone.
    // if (getEnv("NIX_SHOW_SYMBOLS", "0") != "0") {
    //   auto list = topObj.list("symbols");
    //   symbols.dump([&](const std::string& s) { list.elem(s); });
    // }
  }
}

void EvalState::TraceFileAccess(const Path& realPath) {
  if (file_access_trace_fn) {
    if (last_traced_file != realPath) {
      file_access_trace_fn(realPath);
      // Basic deduplication.
      last_traced_file = std::string(realPath);
    }
  }
}

void EvalState::EnableFileAccessTracing(std::function<void(const Path&)> fn) {
  file_access_trace_fn = fn;
}

size_t valueSize(const Value& v) {
  traceable_flat_hash_set<const Bindings*> seenBindings;
  traceable_flat_hash_set<const Env*> seenEnvs;
  traceable_flat_hash_set<const NixList*> seenLists;
  traceable_flat_hash_set<const char*> seenStrings;
  traceable_flat_hash_set<const Value*> seenValues;

  auto doString = [&](const char* s) -> size_t {
    if (seenStrings.find(s) != seenStrings.end()) {
      return 0;
    }
    seenStrings.insert(s);
    return strlen(s) + 1;
  };

  std::function<size_t(const Value& v)> doValue;
  std::function<size_t(const Env& v)> doEnv;

  doValue = [&](const Value& v) -> size_t {
    if (seenValues.find(&v) != seenValues.end()) {
      return 0;
    }
    seenValues.insert(&v);

    size_t sz = sizeof(Value);

    switch (v.type) {
      case tString:
        sz += doString(v.string.s);
        if (v.string.context != nullptr) {
          for (const char** p = v.string.context; *p != nullptr; ++p) {
            sz += doString(*p);
          }
        }
        break;
      case tPath:
        sz += doString(v.path);
        break;
      case tAttrs:
        if (seenBindings.find(v.attrs.get()) == seenBindings.end()) {
          seenBindings.insert(v.attrs.get());
          sz += sizeof(Bindings);
          for (const auto& i : *v.attrs) {
            sz += doValue(*i.second.value);
          }
        }
        break;
      case tList:
        if (seenLists.find(v.list) == seenLists.end()) {
          seenLists.insert(v.list);
          sz += v.listSize() * sizeof(Value*);
          for (const Value* v : *v.list) {
            sz += doValue(*v);
          }
        }
        break;
      case tThunk:
        sz += doEnv(*v.thunk.env);
        break;
      case tApp:
        sz += doValue(*v.app.left);
        sz += doValue(*v.app.right);
        break;
      case tLambda:
        sz += doEnv(*v.lambda.env);
        break;
      case tPrimOpApp:
        sz += doValue(*v.primOpApp.left);
        sz += doValue(*v.primOpApp.right);
        break;
      default:;
    }

    return sz;
  };

  doEnv = [&](const Env& env) -> size_t {
    if (seenEnvs.find(&env) != seenEnvs.end()) {
      return 0;
    }
    seenEnvs.insert(&env);

    size_t sz = sizeof(Env) + sizeof(Value*) * env.size;

    if (env.type != Env::HasWithExpr) {
      for (const Value* v : env.values) {
        if (v != nullptr) {
          sz += doValue(*v);
        }
      }
    } else {
      // TODO(kanepyork): trace ExprWith? how important is this accounting?
    }

    if (env.up != nullptr) {
      sz += doEnv(*env.up);
    }

    return sz;
  };

  return doValue(v);
}

EvalSettings evalSettings;

static GlobalConfig::Register r1(&evalSettings);

}  // namespace nix
