#include "common-eval-args.hh"

#include "download.hh"
#include "eval.hh"
#include "shared.hh"
#include "util.hh"

namespace nix {

MixEvalArgs::MixEvalArgs() {
  mkFlag()
      .longName("arg")
      .description("argument to be passed to Nix functions")
      .labels({"name", "expr"})
      .handler(
          [&](std::vector<std::string> ss) { autoArgs[ss[0]] = 'E' + ss[1]; });

  mkFlag()
      .longName("argstr")
      .description("string-valued argument to be passed to Nix functions")
      .labels({"name", "string"})
      .handler(
          [&](std::vector<std::string> ss) { autoArgs[ss[0]] = 'S' + ss[1]; });

  mkFlag()
      .shortName('I')
      .longName("include")
      .description(
          "add a path to the list of locations used to look up <...> file "
          "names")
      .label("path")
      .handler([&](const std::string& s) { searchPath.push_back(s); });
}

Bindings* MixEvalArgs::getAutoArgs(EvalState& state) {
  Bindings* res = Bindings::NewGC();
  for (auto& i : autoArgs) {
    Value* v = state.allocValue();
    if (i.second[0] == 'E') {
      state.mkThunk_(*v, state.parseExprFromString(std::string(i.second, 1),
                                                   absPath(".")));
    } else {
      mkString(*v, std::string(i.second, 1));
    }
    res->push_back(Attr(state.symbols.Create(i.first), v));
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
