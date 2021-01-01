{ lib, depot, ... }:
# Like mapAttrs, but if `null` is returned from the mapping function,
# the element is removed from the attrset.
#
# mapAttrsMaybe :: (k -> v -> a) -> Attrs k v -> Attrs k a
#
# Examples:
#   mapAttrsMaybe (_: v: v) { foo = null; bar = 42; }
#   => { bar = 42; }
#   mapAttrsMaybe (_: v: if lib.isList v then null else v)
#                 { foo = []; bar = 42; baz = null; }
#   => { bar = 42; }
let
  mapAttrsMaybe = f: attrs: lib.pipe attrs [
    (lib.mapAttrsToList (k: v: { name = k; value = f k v; }))
    (builtins.filter ({name, value}: if value == null then false else true))
    lib.listToAttrs
  ];

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  tests =
    let
      mapId = _: v: v;
      idTest = it "filters out simple nulls over id" [
        (assertEq "empty"
          (mapAttrsMaybe mapId {})
          {})
        (assertEq "onlyNull"
          (mapAttrsMaybe mapId { foo = null; bar = null; })
          {})
        (assertEq "moreStuff"
          (mapAttrsMaybe mapId { foo = null; bar = 42; baz = [ "hi" ]; })
          { bar = 42; baz = [ "hi" ]; })
      ];
      fnTest = it "uses the mapping function to filter out stuff that is null" [
        (assertEq "lists"
          (mapAttrsMaybe (_: v: if lib.isList v then null else v) { foo = []; bar = 42; })
          { bar = 42; })
      ];

    in
      runTestsuite "mapAttrsMaybe" [
        idTest
        fnTest
      ];

in {
  __functor = _: mapAttrsMaybe;

  inherit tests;
}
