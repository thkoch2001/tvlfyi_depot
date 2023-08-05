{ depot, ... }:

let
  inherit (depot.nix.runTestsuite)
    it
    assertEq
    runTestsuite
    ;

  inherit (depot.users.sterni.nix)
    char
    string
    num
    fun
    ;

  charList = string.toChars char.allChars;

  testAllCharConversion = it "tests conversion of all chars" [
    (assertEq "char.chr converts to char.allChars"
      (builtins.genList (fun.rl char.chr (num.add 1)) 255)
      charList)
    (assertEq "char.ord converts from char.allChars"
      (builtins.genList (num.add 1) 255)
      (builtins.map char.ord charList))
  ];

in
runTestsuite "char" [
  testAllCharConversion
]
