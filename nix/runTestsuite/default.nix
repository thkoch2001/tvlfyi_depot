{ lib, pkgs, depot, ... }:

# Run a nix testsuite.
#
# The tests are simple assertions on the nix level,
# and can use derivation outputs if IfD is enabled.
#
# You build a testsuite by bundling assertions into
# “it”s and then bundling the “it”s into a testsuite.
#
# Running the testsuite will abort evaluation if
# any assertion fails.
#
# Example:
#
#   runTestsuite "myFancyTestsuite" [
#     (it "does an assertion" [
#       (assertEq "42 is equal to 42" "42" "42")
#       (assertEq "also 23" 23 23)
#     ])
#     (it "frmbls the brlbr" [
#       (assertEq true false)
#     ])
#   ]
#
# will fail the second it group because true is not false.

let
  inherit (depot.nix.yants)
    sum
    struct
    string
    any
    defun
    list
    drv
    bool
    ;

  bins = depot.nix.getBins pkgs.coreutils [ "printf" "touch" ];

  # rewrite the builtins.partition result
  # to use `ok` and `err` instead of `right` and `wrong`.
  partitionTests = pred: xs:
    let res = builtins.partition pred xs;
    in {
      ok = res.right;
      err = res.wrong;
    };

  AssertErrorContext =
    sum "AssertErrorContext" {
      unexpected-neq = struct "unexpected-neq" {
        left = any;
        right = any;
      };
      unexpected-eval = struct "unexpected-eval" {
        expr = any;
      };
      unexpected-throw = struct "unexpected-throw" { };
    };

  # The result of an assert,
  # either it’s true (yep) or false (nope).
  # If it's nope we return an additional context
  # attribute which gives details on the failure
  # depending on the type of assert performed.
  AssertResult =
    sum "AssertResult" {
      yep = struct "yep" {
        test = string;
      };
      nope = struct "nope" {
        test = string;
        context = AssertErrorContext;
      };
    };

  # Result of an it. An it is a bunch of asserts
  # bundled up with a good description of what is tested.
  ItResult =
    struct "ItResult" {
      it-desc = string;
      asserts = list AssertResult;
    };

  # If the given boolean is true return a positive AssertResult.
  # If the given boolean is false return a negative AssertResult
  # with the provided AssertErrorContext describing the failure.
  # If somethings throws while evaluating the boolean, return
  # an unexpected-throw AssertErrorContext. deepSeq is not used.
  #
  # This function is intended as a generic assert to implement
  # more assert types and is not exposed to the user.
  assertBoolContext = defun [ AssertErrorContext string bool AssertResult ]
    (ctx: desc: res:
      let
        evalResult = builtins.tryEval res;
        successResult = { yep = { test = desc; }; };
        failResult = context: {
          nope = {
            test = desc;
            inherit context;
          };
        };
      in
        if !evalResult.success
        then failResult { unexpected-throw = { }; }
        else if evalResult.value
        then successResult
        else failResult ctx);

  # assert that left and right values are equal
  assertEq = defun [ string any any AssertResult ]
    (desc: left: right:
      let
        ctx = {
          eq = { inherit left right; };
        };
      in assertBoolContext ctx desc (left == right));

  # assert that the expression throws when `deepSeq`-ed
  assertThrows = defun [ string any AssertResult ]
    (desc: expr:
      let
        evalResult = builtins.tryEval (builtins.deepSeq expr {});
        context = { unexpected-eval = { inherit expr; }; };
      in
        if !evalResult.success
        then { yep = { test = desc; }; }
        else { nope = { test = desc; inherit context; }; });

  # assert that the expression does not throw when `deepSeq`-ed
  assertDoesNotThrow = defun [ string any AssertResult ]
    (desc: expr:
      assertBoolContext (throw "true != true") desc (builtins.deepSeq expr true));

  # Annotate a bunch of asserts with a descriptive name
  it = desc: asserts: {
    it-desc = desc;
    inherit asserts;
  };

  # Run a bunch of its and check whether all asserts are yep.
  # If not, abort evaluation with `throw`
  # and print the result of the test suite.
  #
  # Takes a test suite name as first argument.
  runTestsuite = defun [ string (list ItResult) drv ]
    (name: itResults:
      let
        goodAss = ass: {
          good = AssertResult.match ass {
            yep = _: true;
            nope = _: false;
          };
          x = ass;
        };
        goodIt = it: {
          inherit (it) it-desc;
          asserts = partitionTests (ass:
            AssertResult.match ass {
              yep = _: true;
              nope = _: false;
            }) it.asserts;
        };
        goodIts = partitionTests (it: (goodIt it).asserts.err == []);
        res = goodIts itResults;
      in
        if res.err == []
        then depot.nix.runExecline.local "testsuite-${name}-successful" {} [
          "importas" "out" "out"
          "if" [ bins.printf "%s\n" "testsuite ${name} successful!" ]
          bins.touch "$out"
        ]
        else depot.nix.runExecline.local "testsuite-${name}-failed" {} [
          "importas" "out" "out"
          "if" [
            bins.printf "%s\n%s\n"
              "testsuite ${name} failed!"
              (lib.generators.toPretty {} res)
          ]
          "exit" "1"
        ]);

in {
  inherit
    assertEq
    assertThrows
    assertDoesNotThrow
    it
    runTestsuite
    ;
}
