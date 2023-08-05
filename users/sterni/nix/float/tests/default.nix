{ depot, lib, ... }:

let

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  inherit (depot.users.sterni.nix)
    float
    ;

  testsBuiltins = it "tests builtin operations" [
    (assertEq "ceil pos" (float.ceil 1.5) 2)
    (assertEq "ceil neg" (float.ceil (-1.5)) (-1))
    (assertEq "floor pos" (float.floor 1.5) 1)
    (assertEq "floor neg" (float.floor (-1.5)) (-2))
  ];

  testsConversionFrom = it "tests integer to float conversion" [
    (assertEq "float.intToFloat is identity for floats" (float.intToFloat 1.3) 1.3)
    (assertEq "float.intToFloat converts ints"
      (builtins.all
        (val: builtins.isFloat val)
        (builtins.map float.intToFloat (builtins.genList (i: i - 500) 1000)))
      true)
  ];

  exampleFloats = [ 0.5 0.45 0.3 0.1 200 203.457847 204.65547 (-1.5) (-2) (-1.3) (-0.45) ];
  testsConversionTo = it "tests float to integer conversion" [
    (assertEq "round"
      (builtins.map float.round exampleFloats)
      [ 1 0 0 0 200 203 205 (-2) (-2) (-1) 0 ])
    (assertEq "truncate towards zero"
      (builtins.map float.truncate exampleFloats)
      [ 0 0 0 0 200 203 204 (-1) (-2) (-1) 0 ])
  ];
in

runTestsuite "nix.num" ([
  testsConversionFrom
]
  # Skip for e.g. C++ Nix < 2.4
++ lib.optionals (builtins ? ceil && builtins ? floor) [
  testsConversionTo
  testsBuiltins
])
