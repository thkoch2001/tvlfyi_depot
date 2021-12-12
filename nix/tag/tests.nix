{ depot, lib, verifyTag, discr, discrDef, match, matchLam }:

let
  inherit (depot.nix.runTestsuite) runTestsuite assertEq it;

  isTag-test = it "checks whether something is a tag" [
    (assertEq "is Tag" (verifyTag { foo = "bar"; }) {
      isTag = true;
      name = "foo";
      val = "bar";
      errmsg = null;
    })
    (assertEq "is not Tag" (removeAttrs (verifyTag {
      foo = "bar";
      baz = 42;
    }) [ "errmsg" ]) {
      isTag = false;
      name = null;
      val = null;
    })
  ];

  discr-test = it "can discr things" [
    (assertEq "id" (discr [{ a = lib.const true; }] "x") { a = "x"; })
    (assertEq "bools here, ints there"
      (discr [ { bool = lib.isBool; } { int = lib.isInt; } ] 25) { int = 25; })
    (assertEq "bools here, ints there 2"
      (discr [ { bool = lib.isBool; } { int = lib.isInt; } ] true) {
        bool = true;
      })
    (assertEq "fallback to default"
      (discrDef "def" [ { bool = lib.isBool; } { int = lib.isInt; } ] "foo") {
        def = "foo";
      })
  ];

  match-test = it "can match things" [
    (assertEq "match example" (let
      success = { res = 42; };
      failure = { err = "no answer"; };
      matcher = {
        res = i: i + 1;
        err = _: 0;
      };
    in {
      one = match success matcher;
      two = match failure matcher;
    }) {
      one = 43;
      two = 0;
    })
    (assertEq "matchLam & pipe" (lib.pipe { foo = 42; } [
      (matchLam {
        foo = i: if i < 23 then { small = i; } else { big = i; };
        bar = _: { small = 5; };
      })
      (matchLam {
        small = i: "yay it was small";
        big = i: "whoo it was big!";
      })
    ]) "whoo it was big!")
  ];

in runTestsuite "tag" [ isTag-test discr-test match-test ]
