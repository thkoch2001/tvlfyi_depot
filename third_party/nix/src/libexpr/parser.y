%glr-parser
%locations
%define parse.error verbose
%define api.pure true
%defines
/* %no-lines */
%parse-param { void * scanner }
%parse-param { nix::ParseData * data }
%lex-param { void * scanner }
%lex-param { nix::ParseData * data }
%expect 1
%expect-rr 1

%code requires {
#define YY_NO_INPUT 1 // disable unused yyinput features
#include "libexpr/parser.hh"

struct YYSTYPE {
  union {
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
    nix::AttrNameVector * attrNames;
    nix::VectorExprs * string_parts;
  };
};

}

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
    { $$ = new ExprConcatStrings(CUR_POS, false, new nix::VectorExprs({$1, $3})); }
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
  | DOLLAR_CURLY expr '}' { $$ = new nix::VectorExprs; $$->push_back($2); }
  | STR DOLLAR_CURLY expr '}' {
      $$ = new nix::VectorExprs;
      $$->push_back($1);
      $$->push_back($3);
    }
  ;

ind_string_parts
  : ind_string_parts IND_STR { $$ = $1; $1->push_back($2); }
  | ind_string_parts DOLLAR_CURLY expr '}' { $$ = $1; $1->push_back($3); }
  | { $$ = new nix::VectorExprs; }
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
  | attr { $$ = new nix::AttrNameVector; $$->push_back(AttrName(data->symbols.Create($1))); }
  | string_attr
    { $$ = new nix::AttrNameVector;
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
#include "libstore/store-api.hh"


namespace nix {

Expr* EvalState::parse(const char* text, const Path& path, const Path& basePath,
                       StaticEnv& staticEnv) {
  yyscan_t scanner;
  ParseData data(*this);
  data.basePath = basePath;
  data.path = data.symbols.Create(path);

  yylex_init(&scanner);
  yy_scan_string(text, scanner);
  int res = yyparse(scanner, &data);
  yylex_destroy(scanner);

  if (res) {
    throw ParseError(data.error);
  }

  data.result->bindVars(staticEnv);

  return data.result;
}

}  // namespace nix
