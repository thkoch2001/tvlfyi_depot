{ depot, lib, cata, hylo }:

let
  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    ;

  sum = lib.foldl' (a: b: a + b) 0;

  cata-test = it "cata destroys data" [
    (assertEq "attrset"
      (cata
        { fmap = f: v:
            if lib.isAttrs v
            then lib.mapAttrs (_: f) v
            else v;
        }
        (v:
          if lib.isAttrs v
          then sum (lib.attrValues v)
          else v)
        { a.b = 5;
          c = 1;
          x = 6; })
      12)
  ];

  hylo-test = it "hylo builds and then destroys data" [
    (assertEq "attrset"
      (hylo
        { fmap = f: depot.nix.tag.matchLam {
            attrs = as: { attrs = lib.mapAttrs (_: f) as; };
            v = x: { v = x; };
          };
        }
        (depot.nix.tag.matchLam {
          attrs = a: sum (lib.attrValues a);
          v = lib.id;
        })
        (depot.nix.tag.discrDef "v" [
          { attrs = lib.isAttrs; }
        ])
        { a.b = 5;
          c = 1;
          x = 6; })
      12)
  ];



in runTestsuite "rec" [
  cata-test
  hylo-test
]
