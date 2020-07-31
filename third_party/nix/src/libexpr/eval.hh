#pragma once

#include <map>
#include <optional>
#include <unordered_map>
#include <vector>

#include <gc/gc_allocator.h>
#include <gc/gc_cpp.h>

#include "libexpr/attr-set.hh"
#include "libexpr/nixexpr.hh"
#include "libexpr/symbol-table.hh"
#include "libexpr/value.hh"
#include "libutil/config.hh"
#include "libutil/hash.hh"

namespace nix {

class Store;
class EvalState;
enum RepairFlag : bool;

typedef void (*PrimOpFun)(EvalState& state, const Pos& pos, Value** args,
                          Value& v);

struct PrimOp {
  PrimOpFun fun;
  size_t arity;
  Symbol name;
  PrimOp(PrimOpFun fun, size_t arity, Symbol name)
      : fun(fun), arity(arity), name(name) {}
};

struct Env : public gc {
  Env(unsigned short size) : size(size) {
    values = std::vector<Value*, traceable_allocator<Value*>>(size);
  }

  Env* up;
  unsigned short size;           // used by ‘valueSize’
  unsigned short prevWith : 14;  // nr of levels up to next `with' environment
  enum { Plain = 0, HasWithExpr, HasWithAttrs } type : 2;
  std::vector<Value*, traceable_allocator<Value*>> values;
  Expr* withAttrs = nullptr;
};

Value& mkString(Value& v, const std::string& s,
                const PathSet& context = PathSet());

void copyContext(const Value& v, PathSet& context);

/* Cache for calls to addToStore(); maps source paths to the store
   paths. */
typedef std::map<Path, Path> SrcToStore;

std::ostream& operator<<(std::ostream& str, const Value& v);

typedef std::pair<std::string, std::string> SearchPathElem;
typedef std::list<SearchPathElem> SearchPath;

/* Initialise the Boehm GC, if applicable. */
void initGC();

typedef std::map<Path, Expr*, std::less<Path>,
                 traceable_allocator<std::pair<const Path, Expr*>>>
    FileParseCache;

class EvalState : public gc {
 public:
  SymbolTable symbols;

  const Symbol sWith, sOutPath, sDrvPath, sType, sMeta, sName, sValue, sSystem,
      sOutputs, sOutputName, sIgnoreNulls, sFile, sLine, sColumn, sFunctor,
      sToString, sRight, sWrong, sStructuredAttrs, sBuilder, sArgs, sOutputHash,
      sOutputHashAlgo, sOutputHashMode;

  // Symbol representing the path to the built-in 'derivation.nix'
  // file, set during primops initialisation.
  std::optional<Symbol> sDerivationNix;

  /* If set, force copying files to the Nix store even if they
     already exist there. */
  RepairFlag repair;

  /* The allowed filesystem paths in restricted or pure evaluation
     mode. */
  std::optional<PathSet> allowedPaths;

  const ref<Store> store;

 private:
  SrcToStore srcToStore;

  /* A cache from path names to parse trees. */
  FileParseCache fileParseCache;

  /* A cache from path names to values. */
  typedef std::map<Path, Value, std::less<Path>,
                   traceable_allocator<std::pair<const Path, Value>>>
      FileEvalCache;
  FileEvalCache fileEvalCache;

  SearchPath searchPath;

  std::map<std::string, std::pair<bool, std::string>> searchPathResolved;

  /* Cache used by checkSourcePath(). */
  std::unordered_map<Path, Path> resolvedPaths;

 public:
  EvalState(const Strings& _searchPath, const ref<Store>& store);
  ~EvalState();

  void addToSearchPath(const std::string& s);

  SearchPath getSearchPath() { return searchPath; }

  Path checkSourcePath(const Path& path);

  void checkURI(const std::string& uri);

  /* When using a diverted store and 'path' is in the Nix store, map
     'path' to the diverted location (e.g. /nix/store/foo is mapped
     to /home/alice/my-nix/nix/store/foo). However, this is only
     done if the context is not empty, since otherwise we're
     probably trying to read from the actual /nix/store. This is
     intended to distinguish between import-from-derivation and
     sources stored in the actual /nix/store. */
  Path toRealPath(const Path& path, const PathSet& context);

  /* Parse a Nix expression from the specified file. */
  Expr* parseExprFromFile(const Path& path);
  Expr* parseExprFromFile(const Path& path, StaticEnv& staticEnv);

  /* Parse a Nix expression from the specified string. */
  Expr* parseExprFromString(const std::string& s, const Path& basePath,
                            StaticEnv& staticEnv);
  Expr* parseExprFromString(const std::string& s, const Path& basePath);

  Expr* parseStdin();

  /* Evaluate an expression read from the given file to normal
     form. */
  void evalFile(const Path& path, Value& v);

  void resetFileCache();

  /* Look up a file in the search path. */
  Path findFile(const std::string& path);
  Path findFile(SearchPath& searchPath, const std::string& path,
                const Pos& pos = noPos);

  /* If the specified search path element is a URI, download it. */
  std::pair<bool, std::string> resolveSearchPathElem(
      const SearchPathElem& elem);

  /* Evaluate an expression to normal form, storing the result in
     value `v'. */
  void eval(Expr* e, Value& v);

  /* Evaluation the expression, then verify that it has the expected
     type. */
  inline bool evalBool(Env& env, Expr* e);
  inline bool evalBool(Env& env, Expr* e, const Pos& pos);
  inline void evalAttrs(Env& env, Expr* e, Value& v);

  /* If `v' is a thunk, enter it and overwrite `v' with the result
     of the evaluation of the thunk.  If `v' is a delayed function
     application, call the function and overwrite `v' with the
     result.  Otherwise, this is a no-op. */
  inline void forceValue(Value& v, const Pos& pos = noPos);

  /* Force a value, then recursively force list elements and
     attributes. */
  void forceValueDeep(Value& v);

  /* Force `v', and then verify that it has the expected type. */
  NixInt forceInt(Value& v, const Pos& pos);
  NixFloat forceFloat(Value& v, const Pos& pos);
  bool forceBool(Value& v, const Pos& pos);
  inline void forceAttrs(Value& v);
  inline void forceAttrs(Value& v, const Pos& pos);
  inline void forceList(Value& v);
  inline void forceList(Value& v, const Pos& pos);
  void forceFunction(Value& v, const Pos& pos);  // either lambda or primop
  std::string forceString(Value& v, const Pos& pos = noPos);
  std::string forceString(Value& v, PathSet& context, const Pos& pos = noPos);
  std::string forceStringNoCtx(Value& v, const Pos& pos = noPos);

  /* Return true iff the value `v' denotes a derivation (i.e. a
     set with attribute `type = "derivation"'). */
  bool isDerivation(Value& v);

  std::optional<std::string> tryAttrsToString(const Pos& pos, Value& v,
                                              PathSet& context,
                                              bool coerceMore = false,
                                              bool copyToStore = true);

  /* String coercion.  Converts strings, paths and derivations to a
     string.  If `coerceMore' is set, also converts nulls, integers,
     booleans and lists to a string.  If `copyToStore' is set,
     referenced paths are copied to the Nix store as a side effect. */
  std::string coerceToString(const Pos& pos, Value& v, PathSet& context,
                             bool coerceMore = false, bool copyToStore = true);

  std::string copyPathToStore(PathSet& context, const Path& path);

  /* Path coercion.  Converts strings, paths and derivations to a
     path.  The result is guaranteed to be a canonicalised, absolute
     path.  Nothing is copied to the store. */
  Path coerceToPath(const Pos& pos, Value& v, PathSet& context);

 public:
  /* The base environment, containing the builtin functions and
     values. */
  Env& baseEnv;

  /* The same, but used during parsing to resolve variables. */
  StaticEnv staticBaseEnv;  // !!! should be private

 private:
  unsigned int baseEnvDispl = 0;

  void createBaseEnv();

  Value* addConstant(const std::string& name, Value& v);

  Value* addPrimOp(const std::string& name, size_t arity, PrimOpFun primOp);

 public:
  Value& getBuiltin(const std::string& name);

 private:
  inline Value* lookupVar(Env* env, const ExprVar& var, bool noEval);

  friend struct ExprVar;
  friend struct ExprAttrs;
  friend struct ExprLet;

  Expr* parse(const char* text, const Path& path, const Path& basePath,
              StaticEnv& staticEnv);

 public:
  /* Do a deep equality test between two values.  That is, list
     elements and attributes are compared recursively. */
  bool eqValues(Value& v1, Value& v2);

  bool isFunctor(Value& fun);

  void callFunction(Value& fun, Value& arg, Value& v, const Pos& pos);
  void callPrimOp(Value& fun, Value& arg, Value& v, const Pos& pos);

  /* Automatically call a function for which each argument has a
     default value or has a binding in the `args' map. */
  void autoCallFunction(Bindings& args, Value& fun, Value& res);

  /* Allocation primitives. */
  Value* allocValue();
  Env& allocEnv(size_t size);

  Value* allocAttr(Value& vAttrs, const Symbol& name);

  // Create a list value from the specified vector.
  void mkList(Value& v, NixList* list);

  // Create a list value, allocating as many elements as specified in
  // size. This is used for the many cases in this codebase where
  // assignment happens into the preallocated list.
  void mkList(Value& v, size_t size = 0);

  void mkAttrs(Value& v, size_t capacity);
  void mkThunk_(Value& v, Expr* expr);
  void mkPos(Value& v, Pos* pos);

  void concatLists(Value& v, const NixList& lists, const Pos& pos);

  /* Print statistics. */
  void printStats();

  void realiseContext(const PathSet& context);

 private:
  unsigned long nrEnvs = 0;
  unsigned long nrValuesInEnvs = 0;
  unsigned long nrValues = 0;
  unsigned long nrListElems = 0;
  unsigned long nrAttrsets = 0;
  unsigned long nrAttrsInAttrsets = 0;
  unsigned long nrOpUpdates = 0;
  unsigned long nrOpUpdateValuesCopied = 0;
  unsigned long nrListConcats = 0;
  unsigned long nrPrimOpCalls = 0;
  unsigned long nrFunctionCalls = 0;

  bool countCalls;

  typedef std::map<Symbol, size_t> PrimOpCalls;
  PrimOpCalls primOpCalls;

  typedef std::map<ExprLambda*, size_t> FunctionCalls;
  FunctionCalls functionCalls;

  void incrFunctionCall(ExprLambda* fun);

  typedef std::map<Pos, size_t> AttrSelects;
  AttrSelects attrSelects;

  friend struct ExprOpUpdate;
  friend struct ExprOpConcatLists;
  friend struct ExprSelect;
  friend void prim_getAttr(EvalState& state, const Pos& pos, Value** args,
                           Value& v);
};

/* Return a string representing the type of the value `v'. */
std::string showType(const Value& v);

/* Decode a context string ‘!<name>!<path>’ into a pair <path,
   name>. */
std::pair<std::string, std::string> decodeContext(const std::string& s);

/* If `path' refers to a directory, then append "/default.nix". */
Path resolveExprPath(Path path);

struct InvalidPathError : EvalError {
  Path path;
  InvalidPathError(const Path& path);
#ifdef EXCEPTION_NEEDS_THROW_SPEC
  ~InvalidPathError() noexcept {};
#endif
};

struct EvalSettings : Config {
  Setting<bool> restrictEval{
      this, false, "restrict-eval",
      "Whether to restrict file system access to paths in $NIX_PATH, "
      "and network access to the URI prefixes listed in 'allowed-uris'."};

  Setting<bool> pureEval{this, false, "pure-eval",
                         "Whether to restrict file system and network access "
                         "to files specified by cryptographic hash."};

  Setting<bool> enableImportFromDerivation{
      this, true, "allow-import-from-derivation",
      "Whether the evaluator allows importing the result of a derivation."};

  Setting<Strings> allowedUris{
      this,
      {},
      "allowed-uris",
      "Prefixes of URIs that builtin functions such as fetchurl and fetchGit "
      "are allowed to fetch."};

  Setting<bool> traceFunctionCalls{this, false, "trace-function-calls",
                                   "Emit log messages for each function entry "
                                   "and exit at the 'vomit' log level (-vvvv)"};
};

extern EvalSettings evalSettings;

}  // namespace nix
