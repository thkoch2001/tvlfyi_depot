{ depot, pkgs, lib, ... }:
let
  bins = depot.nix.getBins pkgs.coreutils ["printf" "mkdir" "cat" "ln"];

  inherit (depot.nix.yants) defun struct restrict attrs list string drv any;

  FlakeError =
    restrict
      "flake error"
      (s: lib.any (prefix: (builtins.substring 0 1 s) == prefix)
          [ "E" "W" ])
      string;
  Libraries = defun [ (attrs any) (list drv) ];

  python3 = {
    name,
    libraries ? (_: []),
    flakeIgnore ? []
  }: pkgs.writers.writePython3 name {
    libraries = Libraries libraries pkgs.python3Packages;
    flakeIgnore =
      let ignoreTheseErrors = [
        # whitespace after {
        "E201"
        # whitespace before }
        "E202"
        # fuck 4-space indentation
        "E121" "E111"
        # who cares about blank lines …
        # … at end of files
        "W391"
        # … between functions
        "E302" "E305"
      ];
      in list FlakeError (ignoreTheseErrors ++ flakeIgnore);
  };

  # TODO: add the same flake check as the pyhon3 writer
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

  rustSimple = args@{name, ...}: src:
    linkTo name "${rustSimpleBin args src}/bin/${name}";

  linkTo = name: path: depot.nix.runExecline.local name {} [
    "importas" "out" "out"
    bins.ln "-sT" path "$out"
  ];

  rustSimpleBin = {
    name,
    dependencies ? [],
    ...
  }@args: src: pkgs.buildRustCrate ({
      pname = name;
      version = "1.0.0";
      crateName = name;
      crateBin = [ name ];
      dependencies = dependencies;
      src = pkgs.runCommandLocal "write-main.rs" {
        src = src;
        passAsFile = [ "src" ];
      } ''
        mkdir -p $out/src/bin
        cp "$srcPath" $out/src/bin/${name}.rs
        find $out
      '';
    } // args);

  rustSimpleLib = {
    name,
    dependencies ? [],
    ...
  }@args: src: pkgs.buildRustCrate ({
      pname = name;
      version = "1.0.0";
      crateName = name;
      dependencies = dependencies;
      src = pkgs.runCommandLocal "write-lib.rs" {
        src = src;
        passAsFile = [ "src" ];
      } ''
        mkdir -p $out/src
        cp "$srcPath" $out/src/lib.rs
        find $out
      '';
    } // args);

  tests = import ./tests.nix {
    inherit
      depot
      pkgs
      python3
      python3Lib
      rustSimpleLib
      rustSimple
      ;
   };

in {
  inherit
    python3
    python3Lib
    rustSimple
    rustSimpleBin
    rustSimpleLib
    tests
    ;
}
