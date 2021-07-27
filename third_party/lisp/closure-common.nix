{ depot, pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://repo.or.cz/closure-common.git";
    rev = "377f8275f2c90a6fbde800a6c690b33fa166865c"; # 2010-10-06
    sha256 = "0yx9rway1lwvvsflyrsj7a7kjlm05kkhx65jpf1c9dh6pc7r9kjx";
  };

  getSrcs = builtins.map (p: "${src}/${p}");
in

depot.nix.buildLisp.library {
  name = "closure-common";

  # feature specific file inclusions in closure-common.asd;
  # including all relevant fails for SBCL, may not build with
  # other implementations in this form.
  srcs = getSrcs [
    "closure-common.asd"
    "package.lisp"
    "definline.lisp"
    "characters.lisp"
    "syntax.lisp"
    "encodings.lisp"
    "encodings-data.lisp"
    "xstream.lisp"
    "ystream.lisp"
    "hax.lisp"
  ];

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
    depot.third_party.lisp.trivial-gray-streams
    depot.third_party.lisp.babel
  ];
}
