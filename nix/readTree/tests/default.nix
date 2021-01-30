{ depot, lib, ... }:

let
  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    assertThrows
    ;

  tree = depot.nix.readTree {} ./test-tree;

  example = it "corresponds to the example" [
    (assertEq "third_party attrset"
      (lib.isAttrs tree.third_party
      && (! lib.isDerivation tree.third_party))
      true)
    (assertEq "third_party attrset other attribute"
      tree.third_party.favouriteColour
      "orange")
    (assertEq "rustpkgs attrset aho-corasick"
      tree.third_party.rustpkgs.aho-corasick
      "aho-corasick")
    (assertEq "rustpkgs attrset serde"
      tree.third_party.rustpkgs.serde
      "serde")
    (assertEq "tools cheddear"
      "cheddar"
      tree.tools.cheddar)
    (assertEq "tools roquefort"
      tree.tools.roquefort
      "roquefort")
  ];

in runTestsuite "readTree" [
  example
]
