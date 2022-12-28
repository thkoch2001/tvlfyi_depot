{ depot, pkgs, lib, ... }:

let
  inherit (pkgs.buildPackages) cbqn ngn-k;

  # input files are not checked in
  meta.ci.skip = true;

  BQNLIBS = pkgs.fetchFromGitHub {
    owner = "mlochbaum";
    repo = "bqn-libs";
    rev = "d56d8ea0b8c294fac7274678d9ab112553a03f42";
    sha256 = "1c1bkqj62v8m13jgaa32ridy0fk5iqysq5b2qwxbqxhky5zwnk9h";
  };
in

depot.nix.readTree.drvTargets {
  shell = pkgs.mkShell {
    name = "aoc-2022-shell";
    packages = [
      cbqn
      ngn-k
    ];

    inherit BQNLIBS;
  };

  bqn = pkgs.runCommand "bqn-aoc-2022"
    {
      nativeBuildInputs = [
        cbqn
      ];

      aoc = builtins.path {
        name = "bqn-aoc-2022";
        path = ./../.;
        # Need lib.bqn from ../ and all inputs as well as bqn files from ./*
        filter = path: type:
          lib.hasSuffix ".bqn" path || (
            lib.hasPrefix (toString ./.) path
            && (
              type == "directory"
              || lib.hasSuffix "/input" path
            )
          );
      };

      inherit meta BQNLIBS;
    }
    ''
      find "$aoc/2022" -name '*.bqn' -exec BQN {} \; | tee "$out"
    '';
}
