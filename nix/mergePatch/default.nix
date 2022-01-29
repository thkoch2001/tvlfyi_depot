{ lib, depot, ... }:
/*
  JSON Merge-Patch for nix
  Spec: https://tools.ietf.org/html/rfc7396

  An algorithm for changing and removing fields in nested objects.

  For example, given the following original document:

  {
  a = "b";
  c = {
  d = "e";
  f = "g";
  }
  }

  Changing the value of `a` and removing `f` can be achieved by merging the patch

  {
  a = "z";
  c.f = null;
  }

  which results in

  {
  a = "z";
  c = {
  d = "e";
  };
  }

  Pseudo-code:
  define MergePatch(Target, Patch):
  if Patch is an Object:
  if Target is not an Object:
  Target = {} # Ignore the contents and set it to an empty Object
  for each Name/Value pair in Patch:
  if Value is null:
  if Name exists in Target:
  remove the Name/Value pair from Target
  else:
  Target[Name] = MergePatch(Target[Name], Value)
  return Target
  else:
  return Patch
*/

let
  foldlAttrs = op: init: attrs:
    lib.foldl' op init
      (lib.mapAttrsToList lib.nameValuePair attrs);

  mergePatch = target: patch:
    if lib.isAttrs patch
    then
      let target' = if lib.isAttrs target then target else { };
      in
      foldlAttrs
        (acc: patchEl:
          if patchEl.value == null
          then removeAttrs acc [ patchEl.name ]
          else acc // {
            ${patchEl.name} =
              mergePatch
                (acc.${patchEl.name} or "unnused")
                patchEl.value;
          })
        target'
        patch
    else patch;

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  tests =
    let
      # example target from the RFC
      testTarget = {
        a = "b";
        c = {
          d = "e";
          f = "g";
        };
      };
      # example patch from the RFC
      testPatch = {
        a = "z";
        c.f = null;
      };
      emptyPatch = it "the empty patch returns the original target" [
        (assertEq "id"
          (mergePatch testTarget { })
          testTarget)
      ];
      nonAttrs = it "one side is a non-attrset value" [
        (assertEq "target is a value means the value is replaced by the patch"
          (mergePatch 42 testPatch)
          (mergePatch { } testPatch))
        (assertEq "patch is a value means it replaces target alltogether"
          (mergePatch testTarget 42)
          42)
      ];
      rfcExamples = it "the examples from the RFC" [
        (assertEq "a subset is deleted and overwritten"
          (mergePatch testTarget testPatch)
          {
            a = "z";
            c = {
              d = "e";
            };
          })
        (assertEq "a more complicated example from the example section"
          (mergePatch
            {
              title = "Goodbye!";
              author = {
                givenName = "John";
                familyName = "Doe";
              };
              tags = [ "example" "sample" ];
              content = "This will be unchanged";
            }
            {
              title = "Hello!";
              phoneNumber = "+01-123-456-7890";
              author.familyName = null;
              tags = [ "example" ];
            })
          {
            title = "Hello!";
            phoneNumber = "+01-123-456-7890";
            author = {
              givenName = "John";
            };
            tags = [ "example" ];
            content = "This will be unchanged";
          })
      ];

      rfcTests =
        let
          r = index: target: patch: res:
            (assertEq "test number ${toString index}"
              (mergePatch target patch)
              res);
        in
        it "the test suite from the RFC" [
          (r 1 { "a" = "b"; } { "a" = "c"; } { "a" = "c"; })
          (r 2 { "a" = "b"; } { "b" = "c"; } { "a" = "b"; "b" = "c"; })
          (r 3 { "a" = "b"; } { "a" = null; } { })
          (r 4 { "a" = "b"; "b" = "c"; }
            { "a" = null; }
            { "b" = "c"; })
          (r 5 { "a" = [ "b" ]; } { "a" = "c"; } { "a" = "c"; })
          (r 6 { "a" = "c"; } { "a" = [ "b" ]; } { "a" = [ "b" ]; })
          (r 7 { "a" = { "b" = "c"; }; }
            { "a" = { "b" = "d"; "c" = null; }; }
            { "a" = { "b" = "d"; }; })
          (r 8 { "a" = [{ "b" = "c"; }]; }
            { "a" = [ 1 ]; }
            { "a" = [ 1 ]; })
          (r 9 [ "a" "b" ] [ "c" "d" ] [ "c" "d" ])
          (r 10 { "a" = "b"; } [ "c" ] [ "c" ])
          (r 11 { "a" = "foo"; } null null)
          (r 12 { "a" = "foo"; } "bar" "bar")
          (r 13 { "e" = null; } { "a" = 1; } { "e" = null; "a" = 1; })
          (r 14 [ 1 2 ]
            { "a" = "b"; "c" = null; }
            { "a" = "b"; })
          (r 15 { }
            { "a" = { "bb" = { "ccc" = null; }; }; }
            { "a" = { "bb" = { }; }; })
        ];

    in
    runTestsuite "mergePatch" [
      emptyPatch
      nonAttrs
      rfcExamples
      rfcTests
    ];

in
{
  __functor = _: mergePatch;

  inherit tests;
}
