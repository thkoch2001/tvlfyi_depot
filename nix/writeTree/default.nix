{ depot, lib, pkgs, ... }:
let
  inherit (lib) fix pipe mapAttrsToList isAttrs concatLines isString;

  inherit (depot.nix.utils) isDirectory isRegularFile;

  esc = s: lib.escapeShellArg /* ensure paths import into store */ "${s}";

  writeTreeAtPath = path: tree:
    ''
      mkdir -p "$out/"${esc path}
    ''
    + pipe tree [
      (mapAttrsToList (k: v:
        # TODO(sterni): a more discoverable isPathLike would fit into //nix/utils
        # ATTN: This check has the flaw that it accepts paths without context
        # that would not be available in the sandbox!
        if lib.types.path.check v then
          if isRegularFile v then
            "cp --reflink=auto ${esc v} \"$out/\"${esc path}/${esc k}"
          else if isDirectory v then ''
            mkdir -p "$out/"${esc path}
            cp -r --reflink=auto ${esc v} "$out/"${esc path}/${esc k}
          ''
          else
            throw "invalid path type (expected file or directory)"
        else if isAttrs v then
          writeTreeAtPath "${path}/${k}" v
        else if isString v then
          "cp --reflink=auto ${esc v} \"$out/\"${esc path}/${esc k}"
        else
          throw "invalid type (expected file, directory, or attrs)"))
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
