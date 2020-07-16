%glr-parser
%pure-parser
%locations
%define parse.error verbose
%defines
/* %no-lines */
%parse-param { void * scanner }
%parse-param { nix::ParseData * data }
%lex-param { void * scanner }
%lex-param { nix::ParseData * data }
%expect 1
%expect-rr 1

%code requires { #include "libexpr/parser.hh" }

%{

#include "generated/parser-tab.hh"
#include "generated/lexer-tab.hh"

YY_DECL;

using namespace nix;

namespace nix {

static inline Pos makeCurPos(const YYLTYPE& loc, ParseData* data) {
  return Pos(data->path, loc.first_line, loc.first_column);
}

void yyerror(YYLTYPE* loc, yyscan_t scanner, ParseData* data,
             const char* error) {
  data->error = (format("%1%, at %2%") % error % makeCurPos(*loc, data)).str();
}

}

%}

%union {
  // !!! We're probably leaking stuff here.
  nix::Expr * e;
  nix::ExprList * list;
  nix::ExprAttrs * attrs;
  nix::Formals * formals;
  nix::Formal * formal;
  nix::NixInt n;
  nix::NixFloat nf;
  const char * id; // !!! -> Symbol
  char * path;
  char * uri;
  std::vector<nix::AttrName> * attrNames;
  std::vector<nix::Expr *> * string_parts;
}

%type <e> start expr expr_function expr_if expr_op
%type <e> expr_app expr_select expr_simple
%type <list> expr_list
%type <attrs> binds
%type <formals> formals
%type <formal> formal
%type <attrNames> attrs attrpath
%type <string_parts> string_parts_interpolated ind_string_parts
%type <e> string_parts string_attr
%type <id> attr
%token <id> ID ATTRPATH
%token <e> STR IND_STR
%token <n> INT
%token <nf> FLOAT
%token <path> PATH HPATH SPATH
%token <uri> URI
%token IF THEN ELSE ASSERT WITH LET IN REC INHERIT EQ NEQ AND OR IMPL OR_KW
%token DOLLAR_CURLY /* == ${ */
%token IND_STRING_OPEN IND_STRING_CLOSE
%token ELLIPSIS

%right IMPL
%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc '<' '>' LEQ GEQ
%right UPDATE
%left NOT
%left '+' '-'
%left '*' '/'
%right CONCAT
%nonassoc '?'
%nonassoc NEGATE

%%

start: expr { data->result = $1; };

expr: expr_function;

expr_function
  : ID ':' expr_function
    { $$ = new ExprLambda(CUR_POS, data->symbols.Create($1), false, 0, $3); }
  | '{' formals '}' ':' expr_function
    { $$ = new ExprLambda(CUR_POS, data->symbols.Create(""), true, $2, $5); }
  | '{' formals '}' '@' ID ':' expr_function
    { $$ = new ExprLambda(CUR_POS, data->symbols.Create($5), true, $2, $7); }
  | ID '@' '{' formals '}' ':' expr_function
    { $$ = new ExprLambda(CUR_POS, data->symbols.Create($1), true, $4, $7); }
  | ASSERT expr ';' expr_function
    { $$ = new ExprAssert(CUR_POS, $2, $4); }
  | WITH expr ';' expr_function
    { $$ = new ExprWith(CUR_POS, $2, $4); }
  | LET binds IN expr_function
    { if (!$2->dynamicAttrs.empty())
        throw ParseError(format("dynamic attributes not allowed in let at %1%")
            % CUR_POS);
      $$ = new ExprLet($2, $4);
    }
  | expr_if
  ;

expr_if
  : IF expr THEN expr ELSE expr { $$ = new ExprIf($2, $4, $6); }
  | expr_op
  ;

expr_op
  : '!' expr_op %prec NOT { $$ = new ExprOpNot($2); }
  | '-' expr_op %prec NEGATE { $$ = new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__sub")), new ExprInt(0)), $2); }
  | expr_op EQ expr_op { $$ = new ExprOpEq($1, $3); }
  | expr_op NEQ expr_op { $$ = new ExprOpNEq($1, $3); }
  | expr_op '<' expr_op { $$ = new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__lessThan")), $1), $3); }
  | expr_op LEQ expr_op { $$ = new ExprOpNot(new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__lessThan")), $3), $1)); }
  | expr_op '>' expr_op { $$ = new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__lessThan")), $3), $1); }
  | expr_op GEQ expr_op { $$ = new ExprOpNot(new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__lessThan")), $1), $3)); }
  | expr_op AND expr_op { $$ = new ExprOpAnd(CUR_POS, $1, $3); }
  | expr_op OR expr_op { $$ = new ExprOpOr(CUR_POS, $1, $3); }
  | expr_op IMPL expr_op { $$ = new ExprOpImpl(CUR_POS, $1, $3); }
  | expr_op UPDATE expr_op { $$ = new ExprOpUpdate(CUR_POS, $1, $3); }
  | expr_op '?' attrpath { $$ = new ExprOpHasAttr($1, *$3); }
  | expr_op '+' expr_op
    { $$ = new ExprConcatStrings(CUR_POS, false, new std::vector<Expr *>({$1, $3})); }
  | expr_op '-' expr_op { $$ = new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__sub")), $1), $3); }
  | expr_op '*' expr_op { $$ = new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__mul")), $1), $3); }
  | expr_op '/' expr_op { $$ = new ExprApp(CUR_POS, new ExprApp(new ExprVar(data->symbols.Create("__div")), $1), $3); }
  | expr_op CONCAT expr_op { $$ = new ExprOpConcatLists(CUR_POS, $1, $3); }
  | expr_app
  ;

expr_app
  : expr_app expr_select
    { $$ = new ExprApp(CUR_POS, $1, $2); }
  | expr_select { $$ = $1; }
  ;

expr_select
  : expr_simple '.' attrpath
    { $$ = new ExprSelect(CUR_POS, $1, *$3, 0); }
  | expr_simple '.' attrpath OR_KW expr_select
    { $$ = new ExprSelect(CUR_POS, $1, *$3, $5); }
  | /* Backwards compatibility: because Nixpkgs has a rarely used
       function named ‘or’, allow stuff like ‘map or [...]’. */
    expr_simple OR_KW
    { $$ = new ExprApp(CUR_POS, $1, new ExprVar(CUR_POS, data->symbols.Create("or"))); }
  | expr_simple { $$ = $1; }
  ;

expr_simple
  : ID {
      if (strcmp($1, "__curPos") == 0)
          $$ = new ExprPos(CUR_POS);
      else
          $$ = new ExprVar(CUR_POS, data->symbols.Create($1));
  }
  | INT { $$ = new ExprInt($1); }
  | FLOAT { $$ = new ExprFloat($1); }
  | '"' string_parts '"' { $$ = $2; }
  | IND_STRING_OPEN ind_string_parts IND_STRING_CLOSE {
      $$ = stripIndentation(CUR_POS, data->symbols, *$2);
  }
  | PATH { $$ = new ExprPath(absPath($1, data->basePath)); }
  | HPATH { $$ = new ExprPath(getHome() + std::string{$1 + 1}); }
  | SPATH {
      std::string path($1 + 1, strlen($1) - 2);
      $$ = new ExprApp(CUR_POS,
          new ExprApp(new ExprVar(data->symbols.Create("__findFile")),
              new ExprVar(data->symbols.Create("__nixPath"))),
          new ExprString(data->symbols.Create(path)));
  }
  | URI { $$ = new ExprString(data->symbols.Create($1)); }
  | '(' expr ')' { $$ = $2; }
  /* Let expressions `let {..., body = ...}' are just desugared
     into `(rec {..., body = ...}).body'. */
  | LET '{' binds '}'
    { $3->recursive = true; $$ = new ExprSelect(noPos, $3, data->symbols.Create("body")); }
  | REC '{' binds '}'
    { $3->recursive = true; $$ = $3; }
  | '{' binds '}'
    { $$ = $2; }
  | '[' expr_list ']' { $$ = $2; }
  ;

string_parts
  : STR
  | string_parts_interpolated { $$ = new ExprConcatStrings(CUR_POS, true, $1); }
  | { $$ = new ExprString(data->symbols.Create("")); }
  ;

string_parts_interpolated
  : string_parts_interpolated STR { $$ = $1; $1->push_back($2); }
  | string_parts_interpolated DOLLAR_CURLY expr '}' { $$ = $1; $1->push_back($3); }
  | DOLLAR_CURLY expr '}' { $$ = new std::vector<Expr *>; $$->push_back($2); }
  | STR DOLLAR_CURLY expr '}' {
      $$ = new std::vector<Expr *>;
      $$->push_back($1);
      $$->push_back($3);
    }
  ;

ind_string_parts
  : ind_string_parts IND_STR { $$ = $1; $1->push_back($2); }
  | ind_string_parts DOLLAR_CURLY expr '}' { $$ = $1; $1->push_back($3); }
  | { $$ = new std::vector<Expr *>; }
  ;

binds
  : binds attrpath '=' expr ';' { $$ = $1; addAttr($$, *$2, $4, makeCurPos(@2, data)); }
  | binds INHERIT attrs ';'
    { $$ = $1;
      for (auto & i : *$3) {
          auto sym = std::get<Symbol>(i);
          if ($$->attrs.find(sym) != $$->attrs.end()) {
              dupAttr(sym, makeCurPos(@3, data), $$->attrs[sym].pos);
          }
          Pos pos = makeCurPos(@3, data);
          $$->attrs[sym] = ExprAttrs::AttrDef(new ExprVar(CUR_POS, sym), pos, true);
      }
    }
  | binds INHERIT '(' expr ')' attrs ';'
    { $$ = $1;
      /* !!! Should ensure sharing of the expression in $4. */
      for (auto & i : *$6) {
          auto sym = std::get<Symbol>(i);
          if ($$->attrs.find(sym) != $$->attrs.end()) {
            dupAttr(sym, makeCurPos(@6, data), $$->attrs[sym].pos);
          }
          $$->attrs[sym] = ExprAttrs::AttrDef(new ExprSelect(CUR_POS, $4, sym), makeCurPos(@6, data));
      }
    }
  | { $$ = new ExprAttrs; }
  ;

attrs
  : attrs attr { $$ = $1; $1->push_back(AttrName(data->symbols.Create($2))); }
  | attrs string_attr
    { $$ = $1;
      ExprString * str = dynamic_cast<ExprString *>($2);
      if (str) {
          $$->push_back(AttrName(str->s));
          delete str;
      } else
          throw ParseError(format("dynamic attributes not allowed in inherit at %1%")
              % makeCurPos(@2, data));
    }
  | { $$ = new AttrPath; }
  ;

attrpath
  : attrpath '.' attr { $$ = $1; $1->push_back(AttrName(data->symbols.Create($3))); }
  | attrpath '.' string_attr
    { $$ = $1;
      ExprString * str = dynamic_cast<ExprString *>($3);
      if (str) {
          $$->push_back(AttrName(str->s));
          delete str;
      } else {
          $$->push_back(AttrName($3));
      }
    }
  | attr { $$ = new std::vector<AttrName>; $$->push_back(AttrName(data->symbols.Create($1))); }
  | string_attr
    { $$ = new std::vector<AttrName>;
      ExprString *str = dynamic_cast<ExprString *>($1);
      if (str) {
          $$->push_back(AttrName(str->s));
          delete str;
      } else
          $$->push_back(AttrName($1));
    }
  ;

attr
  : ID { $$ = $1; }
  | OR_KW { $$ = "or"; }
  ;

string_attr
  : '"' string_parts '"' { $$ = $2; }
  | DOLLAR_CURLY expr '}' { $$ = $2; }
  ;

expr_list
  : expr_list expr_select { $$ = $1; $1->elems.push_back($2); /* !!! dangerous */ }
  | { $$ = new ExprList; }
  ;

formals
  : formal ',' formals
    { $$ = $3; addFormal(CUR_POS, $$, *$1); }
  | formal
    { $$ = new Formals; addFormal(CUR_POS, $$, *$1); $$->ellipsis = false; }
  |
    { $$ = new Formals; $$->ellipsis = false; }
  | ELLIPSIS
    { $$ = new Formals; $$->ellipsis = true; }
  ;

formal
  : ID { $$ = new Formal(data->symbols.Create($1), 0); }
  | ID '?' expr { $$ = new Formal(data->symbols.Create($1), $3); }
  ;

%%


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "libexpr/eval.hh"
#include "libstore/download.hh"
#include "libstore/store-api.hh"


namespace nix {


Expr * EvalState::parse(const char * text,
    const Path & path, const Path & basePath, StaticEnv & staticEnv)
{
    yyscan_t scanner;
    ParseData data(*this);
    data.basePath = basePath;
    data.path = data.symbols.Create(path);

    yylex_init(&scanner);
    yy_scan_string(text, scanner);
    int res = yyparse(scanner, &data);
    yylex_destroy(scanner);

    if (res) { throw ParseError(data.error); }

    data.result->bindVars(staticEnv);

    return data.result;
}


Path resolveExprPath(Path path)
{
    assert(path[0] == '/');

    /* If `path' is a symlink, follow it.  This is so that relative
       path references work. */
    struct stat st;
    while (true) {
        if (lstat(path.c_str(), &st))
            throw SysError(format("getting status of '%1%'") % path);
        if (!S_ISLNK(st.st_mode)) { break; }
        path = absPath(readLink(path), dirOf(path));
    }

    /* If `path' refers to a directory, append `/default.nix'. */
    if (S_ISDIR(st.st_mode))
        path = canonPath(path + "/default.nix");

    return path;
}


Expr * EvalState::parseExprFromFile(const Path & path)
{
    return parseExprFromFile(path, staticBaseEnv);
}


Expr * EvalState::parseExprFromFile(const Path & path, StaticEnv & staticEnv)
{
    return parse(readFile(path).c_str(), path, dirOf(path), staticEnv);
}


Expr * EvalState::parseExprFromString(const std::string & s, const Path & basePath, StaticEnv & staticEnv)
{
    return parse(s.c_str(), "(std::string)", basePath, staticEnv);
}


Expr * EvalState::parseExprFromString(const std::string & s, const Path & basePath)
{
    return parseExprFromString(s, basePath, staticBaseEnv);
}


Expr * EvalState::parseStdin()
{
    //Activity act(*logger, lvlTalkative, format("parsing standard input"));
    return parseExprFromString(drainFD(0), absPath("."));
}


void EvalState::addToSearchPath(const std::string & s)
{
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


Path EvalState::findFile(const std::string & path)
{
    return findFile(searchPath, path);
}


Path EvalState::findFile(SearchPath & searchPath, const std::string & path, const Pos & pos)
{
    for (auto & i : searchPath) {
        std::string suffix;
        if (i.first.empty())
            suffix = "/" + path;
        else {
            auto s = i.first.size();
            if (path.compare(0, s, i.first) != 0 ||
                (path.size() > s && path[s] != '/'))
                continue;
            suffix = path.size() == s ? "" : "/" + std::string(path, s);
        }
        auto r = resolveSearchPathElem(i);
        if (!r.first) { continue; }
        Path res = r.second + suffix;
        if (pathExists(res)) { return canonPath(res); }
    }
    format f = format(
        "file '%1%' was not found in the Nix search path (add it using $NIX_PATH or -I)"
        + std::string(pos ? ", at %2%" : ""));
    f.exceptions(boost::io::all_error_bits ^ boost::io::too_many_args_bit);
    throw ThrownError(f % path % pos);
}


std::pair<bool, std::string> EvalState::resolveSearchPathElem(const SearchPathElem & elem)
{
    auto i = searchPathResolved.find(elem.second);
    if (i != searchPathResolved.end()) { return i->second; }

    std::pair<bool, std::string> res;

    if (isUri(elem.second)) {
        try {
            CachedDownloadRequest request(elem.second);
            request.unpack = true;
            res = { true, getDownloader()->downloadCached(store, request).path };
        } catch (DownloadError & e) {
          LOG(WARNING) << "Nix search path entry '" << elem.second << "' cannot be downloaded, ignoring";
          res = { false, "" };
        }
    } else {
        auto path = absPath(elem.second);
        if (pathExists(path)) {
            res = { true, path };
        } else {
          LOG(WARNING) << "Nix search path entry '" << elem.second << "' does not exist, ignoring";
          res = { false, "" };
        }
    }

    DLOG(INFO) << "resolved search path element '" << elem.second << "' to '" << res.second << "'";

    searchPathResolved[elem.second] = res;
    return res;
}

}  // namespace nix
