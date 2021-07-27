{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "sharplispers";
    repo = "closure-common";
    rev = "e3c5f5f454b72b01b89115e581c3c52a7e201e5c"; # 2018-09-09
    sha256 = "0k5r2qxn122pxi301ijir3nayi9sg4d7yiy276l36qmzwhp4mg5n";
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
