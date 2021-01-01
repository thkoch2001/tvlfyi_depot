{ depot, pkgs, ... }:
let
  bins = depot.nix.getBins pkgs.coreutils ["printf" "mkdir" "cat"];

  python3 = name: args@{
    libraries ? (_: []),
    flakeIgnore ? []
  }: pkgs.writers.writePython3 name {
    libraries = libraries pkgs.python3Packages;
    flakeIgnore = flakeIgnore;
  };

  python3Lib = { name, libraries ? (_: []) }: moduleString:
    let srcTree = depot.nix.runExecline.local name { stdin = moduleString; } [
      "importas" "out" "out"
      "if" [ bins.mkdir "-p" "\${out}/${name}" ]
      "if" [
        "redirfd" "-w" "1" "\${out}/setup.py"
        bins.printf ''
          from distutils.core import setup

          setup(
            name='%s',
            packages=['%s']
          )
        '' name name
      ]
      "if" [
        # redirect stdin to the init py
        "redirfd" "-w" "1" "\${out}/${name}/__init__.py"
        bins.cat
      ]
    ];
    in pkgs.python3Packages.buildPythonPackage {
      inherit name;
      src = srcTree;
      propagatedBuildInputs = libraries pkgs.python3Packages;
      doCheck = false;
    };

  tests = import ./tests.nix {
    inherit
      depot
      pkgs
      python3
      python3Lib
      ;
   };

in {
  inherit
    python3
    python3Lib
    tests
    ;
}
