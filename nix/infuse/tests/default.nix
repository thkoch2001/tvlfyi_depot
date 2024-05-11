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

  expect-throw = test: (tryEvalDeep test).success == false;

  squared = x: x*x;
  inc = x: x+1;
  inherit (lib) flip;
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
assert expect-throw (infuse { a.b = 3; } { a.b.__init = 9; });

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

assert (infuse 3 []) == 3;

# TODO: test __input
# TODO: test __output
# TODO: test __infuse

#assert expect-throw ((infuse { } { x = {}; }) ? x.y);
#assert let q = (infuse { x = {}; } { x.y = {}; }); in lib.trace (builtins.toJSON q.x.y) (!(q.x?y));
# make sure that infusion.missing cannot "leak out of" the result
#assert ( (infuse { x = 4; } { y = []; }).y == {} );
#assert ( (infuse { x = 4; } { y = z: z; }).y == {} );


##
## Test cases demonstrating non-obvious properties of `infuse`:
##

# identity law of list infusions:
assert
  let
    y-squared = { y = x: x*x;   };
  in
       infuse { y = 4; } [    y-squared [] ]
  ==   infuse { y = 4; } [ [] y-squared    ];

# associativity law of list infusions:
assert
  let
    y-inc     = { y = x: x+1;   };
    y-squared = { y = x: x*x;   };
    y-cubed   = { y = x: x*x*x; };
  in
       infuse { y = 4; } [ [ y-squared   y-cubed ] y-inc ]
  ==   infuse { y = 4; } [   y-squared [ y-cubed   y-inc ] ];

# identity law of attrset infusions:
assert
  let
    y-squared = { y = x: x*x;   };
  in
       infuse { y = 4; } [    y-squared {} ]
  ==   infuse { y = 4; } [ {} y-squared    ];

# distributive laws:
# 1. attributes distribute over lists
# 2. `flip infuse` distributes over lists
assert lib.pipe { y = 4; } (map (flip infuse) [ [ { y = [ squared          inc    ]; } ] ])
  ==   lib.pipe { y = 4; } (map (flip infuse) [ [ { y =   squared; } { y = inc; } ]      ]);
assert lib.pipe { y = 4; } (map (flip infuse) [ [ { y =   squared; } { y = inc; } ]      ])
  ==   lib.pipe { y = 4; } (map (flip infuse) [   { y =   squared; } { y = inc; }        ]);

# simpler example of the first distributive law
assert infuse { y = 4; } [ { y =   squared; } { y = inc;   } ]
  ==   infuse { y = 4; }   { y = [ squared          inc ]; }   ;

# Infusing the empty set at a non-existing attrpath causes that attrpath to be
# created with {} as its value.  This is not ideal; in a perfect world, infusing
# the empty attrset at a non-existing attrpath would do nothing at all.
# Unfortunately there is a very high performance cost to producing that ideal
# behavior while maintaining laziness.
assert (infuse { } { x = {}; }) == { x = {}; };

# make sure the `infuse.missing` value does not leak out to the return value of `infuse`
assert expect-throw (infuse { x = 4; } { y = z: z; }).y;
assert expect-throw (infuse { x = 4; } { y = []; }).y;

# however we can pass `infuse.missing` along a pipeline:
assert (infuse { x = 4; } { y = [ lib.id (_: 3) ]; } == { x = 4; y = 3; });

# we also need to check the "distributive law" here:
assert (infuse { x = 4; } [ { y = lib.id; } { y = _: 3; } ] == { x = 4; y = 3; });
assert (lib.pipe { x = 4; } [ (lib.flip infuse { y = lib.id; }) (lib.flip infuse { y = _: 3; }) ] == { x = 4; y = 3; });

# although you might think that `t: infuse t {}` is the same as `lib.id` that's
# not quite correct: the former must fail when the argument isn't an attrset.
assert expect-throw ((infuse 3 { }) == null);


"all tests passed"



