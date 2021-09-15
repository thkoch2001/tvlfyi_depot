{ depot, ... }:

let
  inherit (depot.nix.runTestsuite)
    runNintTestsuite
    it
    assertEq
    ;

  inherit (depot.users.sterni.nix)
    fun
    ;

  hasEllipsisTests = it "checks fun.hasEllipsis" [
    (assertEq "Malicious string" false
      (fun.hasEllipsis (builtins.toXML ({ foo, ... }: 12))))
    (assertEq "No function" false
      (fun.hasEllipsis 23))
    (assertEq "No attribute set pattern" false
      (fun.hasEllipsis (a: a + 2)))
    (assertEq "No ellipsis" false
      (fun.hasEllipsis ({ foo, bar }: foo + bar)))
    (assertEq "Ellipsis" true
      (fun.hasEllipsis ({ depot, pkgs, ... }: 42)))
  ];
in
  runNintTestsuite "nix.fun" [
    hasEllipsisTests
  ]
