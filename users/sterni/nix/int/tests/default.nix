{ depot, ... }:

let

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  inherit (depot.users.sterni.nix)
    int
    ;

  testBounds = it "checks minBound and maxBound" [
    (assertEq "maxBound is the maxBound" true
      (int.maxBound + 1 < int.maxBound))
    (assertEq "minBound is the minBound" true
      (int.minBound - 1 > int.minBound))
    (assertEq "maxBound overflows to minBound"
      (int.maxBound + 1)
      int.minBound)
    (assertEq "minBound overflows to maxBound"
      (int.minBound - 1)
      int.maxBound)
  ];

in
  runTestsuite "nix.int" [
    testBounds
  ]
