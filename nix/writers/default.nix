{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.s6-portable-utils [ "s6-ln" "s6-ls" "s6-touch" ]
  ;

  linkTo = name: path: depot.nix.runExecline.local name { } [
    "importas"
    "out"
    "out"
    bins.s6-ln
    "-s"
    path
    "$out"
  ];

  # Build a rust executable, $out is the executable.
  rustSimple = args@{ name, ... }: src:
    linkTo name "${rustSimpleBin args src}/bin/${name}";

  # Like `rustSimple`, but put the binary in `$out/bin/`.
  rustSimpleBin =
    { name
    , dependencies ? [ ]
    , doCheck ? true
    ,
    }: src:
    (if doCheck then testRustSimple else pkgs.lib.id)
      (pkgs.buildRustCrate ({
        pname = name;
        version = "1.0.0";
        crateName = name;
        crateBin = [ name ];
        dependencies = dependencies;
        src = pkgs.runCommandLocal "write-main.rs"
          {
            src = src;
            passAsFile = [ "src" ];
          } ''
          mkdir -p $out/src/bin
          cp "$srcPath" $out/src/bin/${name}.rs
          find $out
        '';
      }));

  # Build a rust library, that can be used as dependency to `rustSimple`.
  # Wrapper around `pkgs.buildRustCrate`, takes all its arguments.
  rustSimpleLib =
    { name
    , dependencies ? [ ]
    , doCheck ? true
    ,
    }: src:
    (if doCheck then testRustSimple else pkgs.lib.id)
      (pkgs.buildRustCrate ({
        pname = name;
        version = "1.0.0";
        crateName = name;
        dependencies = dependencies;
        src = pkgs.runCommandLocal "write-lib.rs"
          {
            src = src;
            passAsFile = [ "src" ];
          } ''
          mkdir -p $out/src
          cp "$srcPath" $out/src/lib.rs
          find $out
        '';
      }));

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
      tests = depot.nix.runExecline.local "${rustDrv.name}-tests-run" { } [
        "importas"
        "out"
        "out"
        "if"
        [
          "pipeline"
          [ bins.s6-ls "${crate true}/tests" ]
          "forstdin"
          "-o0"
          "test"
          "importas"
          "test"
          "test"
          "${crate true}/tests/$test"
        ]
        bins.s6-touch
        "$out"
      ];
    in
    depot.nix.drvSeqL [ tests ] (crate false);

in
{
  inherit
    rustSimple
    rustSimpleBin
    rustSimpleLib
    ;
}
