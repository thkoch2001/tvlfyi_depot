{ depot, pkgs, lib, ... }:
let
  bins = depot.nix.getBins pkgs.s6-portable-utils ["s6-mkdir" "s6-cat" "s6-ln" "s6-ls" "s6-touch" ]
      // depot.nix.getBins pkgs.coreutils ["printf" ];

  inherit (depot.nix.yants) defun struct restrict attrs list string drv any;

  inherit (depot.nix) drvSeqL;

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
      "if" [ bins.s6-mkdir "-p" "\${out}/${name}" ]
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
        bins.s6-cat
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
    bins.s6-ln "-s" path "$out"
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

  /* Takes a `buildRustCrate` derivation as an input,
    * builds it with `{ buildTests = true; }` and runs
    * all tests found in its `tests` dir. If they are
    * all successful, `$out` will point to the crate
    * built with `{ buildTests = false; }`, otherwise
    * it will fail to build.
    *
    * See also `nix.drvSeqL` which is used to implement
    * this behavior.
    */
  testRustSimple = rustDrv:
    let
      crate = buildTests: rustDrv.override { inherit buildTests; };
      tests = depot.nix.runExecline.local "${rustDrv.name}-tests-run" {} [
        "importas" "out" "out"
        "if" [
          "pipeline" [ bins.s6-ls "${crate true}/tests" ]
          "forstdin" "-o0" "test"
          "importas" "test" "test"
          "${crate true}/tests/$test"
        ]
        bins.s6-touch "$out"
      ];
    in drvSeqL [ tests ] (crate false);

in {
  inherit
    python3
    python3Lib
    rustSimple
    rustSimpleBin
    rustSimpleLib
    testRustSimple
    ;
}
