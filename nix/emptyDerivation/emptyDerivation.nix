{ stdenv, pkgs, getBins }:

# The empty derivation. All it does is touch $out.
# Basically the unit value for derivations.
#
# In addition to simple test situations which require
# a derivation, we set __functor, so you can call it
# as a function and pass an attrset. The set you pass
# is `//`-merged with the attrset before calling derivation,
# so you can use this to add more fields.

let
  bins = getBins pkgs.s6-portable-utils [ "s6-touch" ]
    // getBins pkgs.execline [ "importas" "exec" ];

  emptiness = {
    name = "empty-derivation";

    # TODO(Profpatsch): can we get system from tvl?
    inherit (stdenv) system;

    builder = bins.exec;
    args = [ bins.importas "out" "out" bins.s6-touch "$out" ];
  };

in (derivation emptiness) // {
  # This allows us to call the empty derivation
  # like a function and override fields/add new fields.
  __functor = _: overrides: derivation (emptiness // overrides);
}
