{ lib ? import <nixpkgs/lib>
, ...
}:
let
  inherit ((import ./.. { inherit lib; }).v1) infuse;
  inherit (builtins) tryEval;

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

"all tests passed"
