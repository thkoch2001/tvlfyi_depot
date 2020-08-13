// Parser utilities for use in parser.y
#pragma once

// TODO(tazjin): Audit these includes, they were in parser.y
#include <optional>
#include <variant>

#include <glog/logging.h>

#include "libexpr/eval.hh"
#include "libexpr/nixexpr.hh"
#include "libutil/util.hh"

#define YY_DECL                                                               \
  int yylex(YYSTYPE* yylval_param, YYLTYPE* yylloc_param, yyscan_t yyscanner, \
            nix::ParseData* data)

#define CUR_POS makeCurPos(*yylocp, data)

namespace nix {

struct ParseData {
  EvalState& state;
  SymbolTable& symbols;
  Expr* result;
  Path basePath;
  std::optional<Symbol> path;
  std::string error;
  Symbol sLetBody;

  ParseData(EvalState& state)
      : state(state),
        symbols(state.symbols),
        sLetBody(symbols.Create("<let-body>")){};
};

// Clang fails to identify these functions as used, probably because
// of some interaction between the lexer/parser codegen and something
// else.
//
// To avoid warnings for that we disable -Wunused-function in this block.

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"

// TODO(tazjin): move dupAttr to anonymous namespace
static void dupAttr(const AttrPath& attrPath, const Pos& pos,
                    const Pos& prevPos) {
  throw ParseError(format("attribute '%1%' at %2% already defined at %3%") %
                   showAttrPath(attrPath) % pos % prevPos);
}

static void dupAttr(Symbol attr, const Pos& pos, const Pos& prevPos) {
  throw ParseError(format("attribute '%1%' at %2% already defined at %3%") %
                   attr % pos % prevPos);
}

void addAttr(ExprAttrs* attrs, AttrPath& attrPath, Expr* e, const Pos& pos);

void addFormal(const Pos& pos, Formals* formals, const Formal& formal);

Expr* stripIndentation(const Pos& pos, SymbolTable& symbols, VectorExprs& es);

Path resolveExprPath(Path path);

// implementations originally from lexer.l

static Expr* unescapeStr(SymbolTable& symbols, const char* s, size_t length) {
  std::string t;
  t.reserve(length);
  char c;
  while ((c = *s++)) {
    if (c == '\\') {
      assert(*s);
      c = *s++;
      if (c == 'n') {
        t += '\n';
      } else if (c == 'r') {
        t += '\r';
      } else if (c == 't') {
        t += '\t';
      } else
        t += c;
    } else if (c == '\r') {
      /* Normalise CR and CR/LF into LF. */
      t += '\n';
      if (*s == '\n') {
        s++;
      } /* cr/lf */
    } else
      t += c;
  }
  return new ExprString(symbols.Create(t));
}

#pragma clang diagnostic pop  // re-enable -Wunused-function

}  // namespace nix
