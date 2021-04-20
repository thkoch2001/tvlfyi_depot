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

  /* Get the basename of a store path without
     the leading hash.

     Type: (path | drv | string) -> string

     Example:
       storePathName ./foo.c
       => "foo.c"

       storePathName (writeText "foo.c" "int main() { return 0; }")
       => "foo.c"

       storePathName "${hello}/bin/hello"
       => "hello"
  */
  storePathName = p:
    if lib.isDerivation p
    then p.name
    else if builtins.isPath p 
    then builtins.baseNameOf p
    else if builtins.isString p
    then
      let
        noStoreDir = lib.removeSuffix "/"
          (lib.removePrefix "${builtins.storeDir}/" p);
        basename = builtins.unsafeDiscardStringContext
          (builtins.baseNameOf p);
      in
        if noStoreDir == basename
        then builtins.substring 33 (builtins.stringLength basename) basename
        else basename
    else builtins.throw "Don't know how to get (base)name of "
      + lib.generators.toPretty {} p;

in {
  inherit
    drvTargets
    storePathName
    ;
}
