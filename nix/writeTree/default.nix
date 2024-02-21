{ depot, lib, pkgs, ... }:
let
  inherit (lib) fix pipe mapAttrsToList isAttrs concatLines isString isDerivation isPath;

  # TODO(sterni): move to //nix/utils with clearer naming and alternative similar to lib.types.path
  isPathLike = value:
    isPath value
    || isDerivation value
    || (isString value && builtins.hasContext value);

  esc = s: lib.escapeShellArg /* ensure paths import into store */ "${s}";

  writeTreeAtPath = path: tree:
    ''
      mkdir -p "$out/"${esc path}
    ''
    + pipe tree [
      (mapAttrsToList (k: v:
        if isPathLike v then
          "cp -R --reflink=auto ${v} \"$out/\"${esc path}/${esc k}"
        else if lib.isAttrs v then
          writeTreeAtPath (path + "/" + k) v
        else
          throw "invalid type (expected path, derivation, string with context, or attrs)"))
      concatLines
    ];

  /* Create a directory tree specified by a Nix attribute set structure.

     Each value in `tree` should either be a file, a directory, or another tree
     attribute set. Those paths will be written to a directory tree
     corresponding to the structure of the attribute set.

     Type: string -> attrSet -> derivation
  */
  writeTree = name: tree:
    pkgs.runCommandLocal name { } (writeTreeAtPath "" tree);
in

# __functor trick so readTree can add the tests attribute
{
  __functor = _: writeTree;
}
