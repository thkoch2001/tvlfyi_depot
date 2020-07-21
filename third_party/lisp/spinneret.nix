{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "ruricolist";
    repo = "spinneret";
    rev = "9db014abca69617530c009f2a61b0baee7702bf1";
    sha256 = "1jy3hz892h06hw8hd9mhpiqym49gl7bswrsdys0cqsz4fd543dd6";
  };

in depot.nix.buildLisp.library {
  name = "spinneret";

  deps = with depot.third_party.lisp; [
    parenscript
    alexandria
    cl-ppcre
    global-vars
    serapeum
    trivial-gray-streams
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "special.lisp"
    "stream.lisp"
    "syntax.lisp"
    "tags.lisp"
    "spinneret.lisp"
    "run.lisp"
    "functions.lisp"
    "compile.lisp"
    "deftag.lisp"
    "dynamic.lisp"
    "ps.lisp"
  ];
}
