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

  # closure-common.asd surpresses some warnings otherwise breaking
  # compilation. Feature macros across implementations:
  #
  # ECL  #+rune-is-character #-rune-is-integer #-x&y-streams-are-stream
  # CCL  #+rune-is-character #-rune-is-integer #-x&y-streams-are-stream
  # SBCL #+rune-is-character #-rune-is-integer #-x&y-streams-are-stream
  #
  # Since all implementations agree, the alternative files aren't encoded here.
  srcs = getSrcs [
    "closure-common.asd"
    "package.lisp"
    "definline.lisp"
    "characters.lisp"     #+rune-is-character
    "syntax.lisp"
    "encodings.lisp"      #-x&y-streams-are-stream
    "encodings-data.lisp" #-x&y-streams-are-stream
    "xstream.lisp"        #-x&y-streams-are-stream
    "ystream.lisp"        #-x&y-streams-are-stream
    "hax.lisp"
  ];

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
    depot.third_party.lisp.trivial-gray-streams
    depot.third_party.lisp.babel #+rune-is-character
  ];
}
