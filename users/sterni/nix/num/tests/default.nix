{ depot, ... }:

let

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  inherit (depot.users.sterni.nix)
    num
    ;

  testsBasic = it "tests basic operations" [
    (assertEq "abs -4959" (num.abs (-4959)) 4959)
    (assertEq "sum" (num.sum [ 123 321 1.5 ]) (123 + 321 + 1.5))
    (assertEq "inRange"
      (builtins.map (num.inRange 1.0 5) [ 0 0.5 3 4 4.5 5.5 5 6 ])
      [ false false true true true false true false ])
  ];
in

runTestsuite "nix.num" [
  testsBasic
]
