{ depot, lib, isTag, discr, discrDef }:

let
  inherit (depot.nix.runTestsuite)
    runTestsuite
    assertEq
    it
    ;

  isTag-test = it "checks whether something is a tag" [
    (assertEq "is Tag"
      (isTag { foo = "bar"; })
      {
        isTag = true;
        name = "foo";
        val = "bar";
        errmsg = null;
      })
    (assertEq "is not Tag"
      (removeAttrs (isTag { foo = "bar"; baz = 42; }) ["errmsg"])
      {
        isTag = false;
        name = null;
        val = null;
      })
  ];

  discr-test = it "can discr things" [
    (assertEq "id"
      (discr [
        { a = lib.const true; }
      ] "x")
      { a = "x"; })
    (assertEq "bools here, ints there"
      (discr [
        { bool = lib.isBool; }
        { int = lib.isInt; }
      ] 25)
      { int = 25; })
    (assertEq "bools here, ints there 2"
      (discr [
        { bool = lib.isBool; }
        { int = lib.isInt; }
      ] true)
      { bool = true; })
    (assertEq "fallback to default"
      (discrDef "def" [
        { bool = lib.isBool; }
        { int = lib.isInt; }
      ] "foo")
      { def = "foo"; })
  ];

in
  runTestsuite "tag" [
    isTag-test
    discr-test
  ]
