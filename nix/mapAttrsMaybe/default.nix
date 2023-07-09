{ lib, depot, ... }:
# Like mapAttrs, but if `null` is returned from the mapping function,
# the element is removed from the attrset.
#
# mapAttrsMaybe :: (drop -> k -> v -> a) -> Attrs k v -> Attrs k a
#
# Examples:
#   mapAttrsMaybe (drop: _: v: v) { foo = null; bar = 42; }
#   => { bar = 42; foo = null; }
#   mapAttrsMaybe (drop: _: v: if lib.isList v then drop else v)
#                 { foo = []; bar = 42; baz = null; }
#   => { bar = 42; baz = null; }
let

  # this should be upstreamed into nixpkgs as lib.trivial.gensym,
  # and replaced with builtins.gensym at some far-future date
  gensym = ignored: {
    inner = _: {
      inherit ignored;
      touch_me_and_die = throw "you tried to force gensym, fool";
    };
  };

  mapAttrsMaybe = f: attrs:
    let drop = gensym true;
    in lib.pipe attrs [
      (lib.mapAttrsToList (k: v: { name = k; value = f drop k v; }))
      (builtins.filter ({ name, value }: if value == drop then false else true))
      lib.listToAttrs
    ];

  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  tests =
    let
      mapId = drop: _: v: v;
      idTest = it "filters out simple nulls over id" [
        (assertEq "empty"
          (mapAttrsMaybe mapId { })
          { })
        (assertEq "onlyNull"
          (mapAttrsMaybe mapId { foo = null; bar = null; })
          { foo = null; bar = null; })
        (assertEq "moreStuff"
          (mapAttrsMaybe mapId { foo = null; bar = 42; baz = [ "hi" ]; })
          { foo = null; bar = 42; baz = [ "hi" ]; })
      ];
      fnTest = it "uses the mapping function to filter out stuff that is a list" [
        (assertEq "lists"
          (mapAttrsMaybe (drop: _: v: if lib.isList v then drop else v) { foo = [ ]; bar = 42; })
          { bar = 42; })
      ];

    in
    runTestsuite "mapAttrsMaybe" [
      idTest
      fnTest
    ];

in
{
  __functor = _: mapAttrsMaybe;

  meta.ci.targets = ["tests"];
}
