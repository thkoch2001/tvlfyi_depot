{ depot, briefcase, ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/tpapp/let-plus.git";
    rev = "7cf18b29ed0fe9c667a9a6a101b08ab9661a59e9";
  };
in depot.nix.buildLisp.library {
  name = "let-plus";
  deps = [
    depot.third_party.lisp.alexandria
    briefcase.third_party.lisp.anaphora
  ];
  srcs = [
    "${src}/package.lisp"
    "${src}/let-plus.lisp"
    "${src}/extensions.lisp"
  ];
}
