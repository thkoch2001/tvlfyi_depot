#include "libexpr/common-eval-args.hh"

#include "libexpr/eval.hh"
#include "libmain/shared.hh"
#include "libstore/download.hh"
#include "libutil/util.hh"

namespace nix {

MixEvalArgs::MixEvalArgs() {
  mkFlag()
      .longName("arg")
      .description("argument to be passed to Nix functions")
      .labels({"name", "expr"})
      .handler([&](std::vector<std::string> ss) {
        auto_args_[ss[0]] = std::make_pair(kArgTypeExpr, ss[1]);
      });

  mkFlag()
      .longName("argstr")
      .description("string-valued argument to be passed to Nix functions")
      .labels({"name", "string"})
      .handler([&](std::vector<std::string> ss) {
        auto_args_[ss[0]] = std::make_pair(kArgTypeString, ss[1]);
      });

  mkFlag()
      .shortName('I')
      .longName("include")
      .description(
          "add a path to the list of locations used to look up <...> file "
          "names")
      .label("path")
      .handler([&](const std::string& s) { searchPath.push_back(s); });
}

std::unique_ptr<Bindings> MixEvalArgs::getAutoArgs(EvalState& state) {
  auto res = Bindings::New(auto_args_.size());
  for (auto& [arg, arg_value] : auto_args_) {
    Value* v = state.allocValue();
    switch (arg_value.first) {
      case kArgTypeExpr: {
        state.mkThunk_(
            *v, state.parseExprFromString(arg_value.second, absPath(".")));
        break;
      }
      case kArgTypeString: {
        mkString(*v, arg_value.second);
        break;
      }
    }

    res->push_back(Attr(state.symbols.Create(arg), v));
  }
  return res;
}

Path lookupFileArg(EvalState& state, std::string s) {
  if (isUri(s)) {
    CachedDownloadRequest request(s);
    request.unpack = true;
    return getDownloader()->downloadCached(state.store, request).path;
  }
  if (s.size() > 2 && s.at(0) == '<' && s.at(s.size() - 1) == '>') {
    Path p = s.substr(1, s.size() - 2);
    return state.findFile(p);
  } else {
    return absPath(s);
  }
}

}  // namespace nix
