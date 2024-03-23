{ depot, pkgs, ... }:

let
  src = with pkgs; srcOnly lispPackages.trivial-with-current-source-form;
  getSrc = (f: src + ("/code/" + f));
in
depot.nix.buildLisp.library {
  name = "trivial-with-current-source-form;";

  srcs = map getSrc [ "package.lisp" "macro.lisp" ]
    ++ [{ sbcl = getSrc "sbcl.lisp"; } { clasp = getSrc "clasp.lisp"; }];
}
