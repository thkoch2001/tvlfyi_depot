{ emptyDerivation, getBins, pkgs, writeExecline, runTestsuite, it, assertEq }:

let
  bins = getBins pkgs.s6-portable-utils [ "s6-echo" ];

  empty = it "is just an empty path" [
    (assertEq "path empty"
      (builtins.readFile emptyDerivation)
      "")
  ];

  fooOut = emptyDerivation {
    builder = writeExecline "foo-builder" { } [
      "importas"
      "out"
      "out"
      "redirfd"
      "-w"
      "1"
      "$out"
      bins.s6-echo
      "-n"
      "foo"
    ];
  };

  overrideBuilder = it "can override the builder" [
    (assertEq "output is foo"
      (builtins.readFile fooOut)
      "foo")
    (assertEq "can add new drv variables"
      (emptyDerivation { foo = "bar"; }).foo
      "bar")
  ];

in
runTestsuite "emptyDerivation" [
  empty
  overrideBuilder
]
