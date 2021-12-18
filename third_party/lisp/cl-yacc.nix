{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "jech";
  repo = "cl-yacc";
  rev = "1334f5469251ffb3f8738a682dc8ee646cb26635";
  sha256 = "16946pzf8vvadnyfayvj8rbh4zjzw90h0azz2qk1mxrvhh5wklib";
};
in
depot.nix.buildLisp.library {
  name = "cl-yacc";

  srcs = map (f: src + ("/" + f)) [
    "yacc.lisp"
  ];
}
