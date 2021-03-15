{ depot, lib, ... }:

let

  /* Takes an attribute set and adds a meta.targets
     attribute to it which contains all direct children
     of the attribute set which are derivations.

     Type: attrs -> attrs
  */
  drvTargets = attrs:
    attrs // {
      meta = {
        targets = builtins.filter
          (x: lib.isDerivation attrs."${x}")
          (builtins.attrNames attrs);
      } // (attrs.meta or {});
    };

in {
  inherit
    drvTargets
    ;
}
