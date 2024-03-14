{ depot, ... }:

let
  inherit (depot.nix.runTestsuite) runTestsuite it assertEq;

  inherit (depot.nix) escapeExecline;

  inherit (depot.users.sterni.nix) fun;

  hasEllipsisTests = it "checks fun.hasEllipsis" [
    (assertEq "Malicious string" false (fun.hasEllipsis (builtins.toXML ({ foo, ... }: 12))))
    (assertEq "No function" false (fun.hasEllipsis 23))
    (assertEq "No attribute set pattern" false (fun.hasEllipsis (a: a + 2)))
    (assertEq "No ellipsis" false (fun.hasEllipsis ({ foo, bar }: foo + bar)))
    (assertEq "Ellipsis" true (fun.hasEllipsis ({ depot, pkgs, ... }: 42)))
  ];

  argCountTests = it "checks fun.argCount" [
    (assertEq "builtins.sub has two arguments" 2 (fun.argCount builtins.sub))
    (assertEq "fun.argCount has one argument" 1 (fun.argCount fun.argCount))
    (assertEq "runTestsuite has two arguments" 2 (fun.argCount runTestsuite))
  ];

  applyTests = it "checks that fun.apply is equivalent to calling" [
    (assertEq "fun.apply builtins.sub" (builtins.sub 23 42) (
      fun.apply builtins.sub [
        23
        42
      ]
    ))
    (assertEq "fun.apply escapeExecline"
      (escapeExecline [
        "foo"
        [ "bar" ]
      ])
      (
        fun.apply escapeExecline [
          [
            "foo"
            [ "bar" ]
          ]
        ]
      )
    )
  ];

  unapplyTests = it "checks fun.unapply" [
    (assertEq "fun.unapply 3 accepts 3 args" 3 (fun.argCount (fun.unapply 3 fun.id)))
    (assertEq "fun.unapply 73 accepts 73 args" 73 (fun.argCount (fun.unapply 73 fun.id)))
    (assertEq "fun.unapply 1 accepts 73 args" 1 (fun.argCount (fun.unapply 1 fun.id)))
    (assertEq "fun.unapply collects arguments correctly" (fun.unapply 5 fun.id 1 2 3 4 5) [
      1
      2
      3
      4
      5
    ])
    (assertEq "fun.unapply calls the given function correctly" 1 (fun.unapply 1 builtins.head 1))
  ];

  fac' =
    self: acc: n:
    if n == 0 then acc else self (n * acc) (n - 1);

  facPlain = fun.fix fac' 1;
  facOpt = fun.tailCallOpt fac' 1;

  tailCallOptTests = it "checks fun.tailCallOpt" [
    (assertEq "optimized and unoptimized factorial have the same base case" (facPlain 0) (facOpt 0))
    (assertEq "optimized and unoptimized factorial have same value for 1" (facPlain 1) (facOpt 1))
    (assertEq "optimized and unoptimized factorial have same value for 100" (facPlain 100) (facOpt 100))
  ];
in
runTestsuite "nix.fun" [
  hasEllipsisTests
  argCountTests
  applyTests
  unapplyTests
  tailCallOptTests
]
