{ lib ? import <nixpkgs/lib>
, pkgs ? import <nixpkgs> { }
, ...
}:
let
  inherit ((import ./.. { inherit lib; }).v1) infuse;
  inherit (builtins) tryEval deepSeq;

  tryEvalDeep = expr: tryEval (deepSeq expr expr);

  dummy-drv = builtins.derivation { name = "foo"; system = builtins.currentSystem; };

  old = {
    foo.update = 2;
  };

  new = {
    foo.update = x: x + 1;
    foo.newattr = _: 9;
  };

  result = infuse old new;
in

assert result.foo.newattr == 9;
assert result.foo.update == 3;

# tutorial examples
assert        ({ bob.fred = 3; } // { bob.jill = 4; }) == { bob.jill = 4; };
assert (infuse { bob.fred = 3; } { bob.jill = _: 4; }) == { bob.fred = 3; bob.jill = 4; };
assert (infuse { bob.fred = 3; } { bob = _: { jill = 4; }; }) == { bob.jill = 4; };
assert (infuse { bob.fred = 3; } { bob.fred = [ (x: x + 1) (x: x * x) ]; }) == { bob.fred = 16; };
assert (infuse { bob.fred.x = 3; } { bob.fred = [{ x = x: x * x; } (fred: fred.x + 1)]; }) == { bob.fred = 10; };

# function
assert (infuse { a = 3; } (lib.mapAttrs (_: v: v * v))) == { a = 9; };

# list
assert (infuse { a.b = 3; } { a.b = [ (x: x + 1) (x: x * x) ]; }) == { a.b = 16; };

# __replace
assert (infuse { a.b = 3; } { a.b.__replace = 9; }) == { a.b = 9; };
assert (infuse { a.b = 3; } { a.__replace = 9; }) == { a = 9; };

# __default
assert (infuse { a.b = 3; } { a.b.__default = 9; }) == { a.b = 3; };
assert (infuse { a.b = 3; } { a.q.__default = 9; }) == { a.b = 3; a.q = 9; };

# __init
assert (infuse { a.b = 3; } { a.q.__init = 9; }) == { a.b = 3; a.q = 9; };
assert (tryEvalDeep (infuse { a.b = 3; } { a.b.__init = 9; })).success == false;

# __prepend
assert (infuse { } { a.b.__prepend = "fred"; }) == { a.b = "fred"; };
assert (infuse { a.b = "bob"; } { a.b.__prepend = "fred"; }) == { a.b = "fredbob"; };
assert (infuse { } { a.b.__prepend = [ "fred" ]; }) == { a.b = [ "fred" ]; };
assert (infuse { a.b = [ "bob" ]; } { a.b.__prepend = [ "fred" ]; }) == { a.b = [ "fred" "bob" ]; };

# __append
assert (infuse { } { a.b.__append = "fred"; }) == { a.b = "fred"; };
assert (infuse { a.b = "bob"; } { a.b.__append = "fred"; }) == { a.b = "bobfred"; };
assert (infuse { } { a.b.__append = [ "fred" ]; }) == { a.b = [ "fred" ]; };
assert (infuse { a.b = [ "bob" ]; } { a.b.__append = [ "fred" ]; }) == { a.b = [ "bob" "fred" ]; };

# TODO: test __input
# TODO: test __output
# TODO: test __infuse


"all tests passed"

