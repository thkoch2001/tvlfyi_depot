{ depot, ... }:

let

  inherit (depot.users.sterni.nix)
    string
    ;

  inherit (depot.nix.runTestsuite)
    it
    assertEq
    runTestsuite
    ;

  testTakeDrop = it "tests take and drop" [
    (assertEq "take"
      (string.take 5 "five and more")
      "five ")
    (assertEq "drop"
      (string.drop 2 "coin")
      "in")
    (assertEq "take out of bounds"
      (string.take 100 "foo")
      "foo")
    (assertEq "drop out of bounds"
      (string.drop 42 "lol")
      "")
  ];

  testIndexing = it "tests string indexing" [
    (assertEq "normal charAt"
      (string.charAt 3 "helo")
      "o")
    (assertEq "out of bounds charAt"
      (string.charAt 5 "helo")
      null)
  ];

  testFinding = it "tests finding in strings" [
    (assertEq "normal charIndex"
      (string.charIndex "d" "abcdefghijkl")
      3)
    (assertEq "charIndex no match"
      (string.charIndex "w" "zZzZzzzZZZ")
      null)
  ];

  dontEval = builtins.throw "this should not get evaluated";

  testMatch = it "tests match" [
    (assertEq "basic match usage" 42
      (string.match "answer" {
        "answer" = 42;
        "banana" = dontEval;
        "maleur" = dontEval;
      }))
  ];

  f = "f";
  testPrintf = it "prints f" [
    (assertEq "basic %s usage" "print ${f}" (string.printf "print %s" f))
    (assertEq "% escaping" "100%" (string.printf "100%%"))
  ];

in
runTestsuite "nix.string" [
  testTakeDrop
  testIndexing
  testFinding
  testMatch
  testPrintf
]
