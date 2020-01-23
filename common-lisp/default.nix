{ tpkgs ? (import (builtins.fetchGit "https://git.tazj.in/") {}), ... }:

tpkgs.nix.buildLisp.program {
  name = "unit-testing";

  deps = with tpkgs.third_party.lisp; [
    (import ./prove.nix {})
  ];

  srcs = [
    ./unit-testing.lisp
  ];
}
