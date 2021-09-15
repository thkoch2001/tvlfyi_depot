{ depot, ... }:

let
  inherit (depot.nix.runTestsuite)
    it
    assertEq
    runNintTestsuite
    ;

  inherit (depot.users.sterni.nix)
    char
    string
    int
    fun
    ;

  charList = string.toChars char.allChars;

  testAllCharConversion = it "tests conversion of all chars" [
    (assertEq "char.chr converts to char.allChars"
      (builtins.genList (fun.rl char.chr (int.add 1)) 255)
      charList)
    (assertEq "char.ord converts from char.allChars"
      (builtins.genList (int.add 1) 255)
      (builtins.map char.ord charList))
  ];

in
  runNintTestsuite "char" [
    testAllCharConversion
  ]
