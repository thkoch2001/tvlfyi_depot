{
  depot ? import <depot> {},
  universe ? import <universe> {},
  ...
}:

let
  src = builtins.fetchGit {
    url = "https://github.com/tpapp/cl-colors.git";
    rev = "827410584553f5c717eec6182343b7605f707f75";
  };
in depot.nix.buildLisp.library {
  name = "cl-colors";
  deps = [
    depot.third_party.lisp.alexandria
    universe.third_party.lisp.let-plus
  ];
  srcs = [
    "${src}/package.lisp"
    "${src}/colors.lisp"
    "${src}/colornames.lisp"
    "${src}/hexcolors.lisp"
  ];
}
