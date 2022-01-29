{ depot, ... }:

let

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    assertThrows
    ;

  inherit (depot.users.sterni.nix.flow)
    cond
    match
    ;

  dontEval = builtins.throw "this should not get evaluated";

  testCond = it "tests cond" [
    (assertThrows "malformed cond list"
      (cond [ [ true 1 2 ] [ false 1 ] ]))
    (assertEq "last is true" "last"
      (cond [
        [ false dontEval ]
        [ false dontEval ]
        [ true "last" ]
      ]))
    (assertEq "first is true" 1
      (cond [
        [ true 1 ]
        [ true dontEval ]
        [ true dontEval ]
      ]))
  ];

in
runTestsuite "nix.flow" [
  testCond
]
