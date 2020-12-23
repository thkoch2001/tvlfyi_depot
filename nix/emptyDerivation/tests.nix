{ emptyDerivation, getBins, pkgs, writeExecline, runTestsuite, it, assertEq }:

let
  bins = getBins pkgs.s6-portable-utils [ "s6-echo" ];

  empty = it "is just an empty path" [
    (assertEq "path empty"
      (builtins.readFile emptyDerivation)
      "")
  ];

  fooOut = emptyDerivation {
    builder = writeExecline "foo-builder" {} [
      "importas" "out" "out"
      "redirfd" "-w" "1" "$out"
      bins.s6-echo "-n" "foo"
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

  extraAttribute = it "extra attribute" [
    (assertEq "extra does not change store hash"
      (emptyDerivation { extra = { foo = 42; }; })
      (emptyDerivation {}))
    (assertEq "extra can be empty"
      (emptyDerivation { extra = { }; bar = "foo"; })
      (emptyDerivation { bar = "foo"; }))
    (assertEq "extra will always be in the final result even if not given"
      (emptyDerivation { }).extra
      {})
    (assertEq "extra will evaluate to itself"
      (emptyDerivation { extra = { foo = 42; }; }).extra
      { foo = 42; })
   ];

in runTestsuite "emptyDerivation" [
  empty
  overrideBuilder
  extraAttribute
]
