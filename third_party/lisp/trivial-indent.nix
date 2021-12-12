{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "Shinmera";
    repo = "trivial-indent";
    rev = "2d016941751647c6cc5bd471751c2cf68861c94a";
    sha256 = "1sj90nqz17w4jq0ixz00gb9g5g6d2s7l8r17zdby27gxxh51w266";
  };
in depot.nix.buildLisp.library {
  name = "trivial-indent";

  srcs = map (f: src + ("/" + f)) [ "indent.lisp" ];
}
