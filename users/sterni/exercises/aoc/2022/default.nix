{ depot, pkgs, lib, ... }:

let
  inherit (pkgs.buildPackages) cbqn;

  # input files are not checked in
  meta.ci.skip = true;
in

depot.nix.readTree.drvTargets {
  shell = pkgs.mkShell {
    name = "aoc-2022-shell";
    packages = [
      cbqn
      depot.tvix.eval
    ];
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

      inherit meta;
    }
    ''
      find "$aoc/2022" -name '*.bqn' -exec BQN {} \; | tee "$out"
    '';

  nix = import ./nix.nix { inherit lib; };

  tvixed-nix = pkgs.runCommand "tvix-aoc-2022"
    {
      nativeBuildInputs = [
        depot.tvix.eval
      ];
      solutions = builtins.path {
        name = "nix-aoc-2022";
        path = ./.;
        filter = path: type:
          type == "directory"
          || lib.hasSuffix "nix.nix" path
          || lib.hasSuffix "/input" path;
      };

      inherit meta;
    }
    ''
      tvix-eval -E "import $solutions/nix.nix { lib = import ${pkgs.path}/lib; }" \
        | tee "$out"
    '';
}
