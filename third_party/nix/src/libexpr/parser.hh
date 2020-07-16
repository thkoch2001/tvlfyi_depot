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

}  // namespace nix
