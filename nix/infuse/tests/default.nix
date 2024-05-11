let
  lib = import <nixpkgs/lib>;
  inherit (import ./.. { inherit lib; }) weave finish delete;
  finish-weave = strands: finish (weave strands);

  dummy-drv = builtins.derivation { name = "foo"; system = builtins.currentSystem; };

  old = {
    foo.update = 2;

    foo.conflict  = 3;
    foo.conflict2 = 3;
    foo.conflict3 = {};
    foo.conflict4 = dummy-drv;
    foo.conflict5 = { foo = 5; };

    foo.deleted = 9;
  };

  new = {
    foo.update = x: x+1;
    foo.newattr = 9;

    foo.conflict  = 1;
    foo.conflict2 = {};
    foo.conflict3 = 1;
    foo.conflict4 = { foo = 3; };
    foo.conflict5 = dummy-drv;
  };

  result = finish (weave [ old new ]);
  inherit (builtins) tryEval;
in

assert result.foo.newattr == 9;
assert result.foo.update == 3;
# test conflicting updates
assert !(tryEval result.foo.conflict).success;  # unequal ints
assert !(tryEval result.foo.conflict2).success; # attrset vs int
assert !(tryEval result.foo.conflict3).success; # int vs attrset
assert !(tryEval result.foo.conflict4).success; # derivation vs attrset
assert !(tryEval result.foo.conflict5).success; # attrset vs derivation

assert !(result?deleted);

"all tests passed"
