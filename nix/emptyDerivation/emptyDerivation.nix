{ stdenv, pkgs, getBins }:

# The empty derivation. All it does is touch $out.
# Basically the unit value for derivations.
#
# In addition to simple test situations which require
# a derivation, we set __functor, so you can call it
# as a function and pass an attrset. The set you pass
# is `//`-merged with the attrset before calling derivation,
# so you can use this to add more fields.
#
# The special `extra` attribute is not passed to `derivation`,
# thus it can be used to pass some extra fields.
# This is differs from `passthru` in nixpkgs, in that it does
# *not* add the fields directly to the derivation, but only `extra`.
# In the future, we might standardize the extra fields.

let
  bins = getBins pkgs.s6-portable-utils [ "s6-touch" ]
      // getBins pkgs.execline [ "importas" "exec" ];

  emptiness = {
    name = "empty-derivation";

    # TODO(Profpatsch): can we get system from tvl?
    inherit (stdenv) system;

    builder = bins.exec;
    args = [
      bins.importas "out" "out"
      bins.s6-touch "$out"
    ];
  };

  drv = { extra ? {}, ... }@overrides:
    let noExtra = builtins.removeAttrs overrides [ "extra" ];
    in (derivation (emptiness // noExtra))
          // { inherit extra; };


in (drv {}) // {
  # This allows us to call the empty derivation
  # like a function and override fields/add new fields.
  __functor = _: overrides: drv overrides;
}
