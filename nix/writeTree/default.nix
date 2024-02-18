{ depot, lib, pkgs, ... }:
let
  inherit (lib) fix pipe mapAttrsToList isAttrs concatLines isString;

  inherit (depot.nix.utils) isDirectory isRegularFile;

  writeTreeAtPath = path: tree:
    pipe tree [
      (mapAttrsToList (k: v:
        if isRegularFile v then
          "cp ${v} $out/${path}${k}"
        else if isDirectory v then ''
          mkdir -p $out/${path}
          cp -r ${v} $out/${path}${k}
        '' else if isAttrs v then
          writeTreeAtPath "${path}/${k}" v
        else if isString v then
          "cp ${v} $out/${path}${k}"
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
in
name: tree:
pkgs.runCommandLocal name { } ''
  mkdir $out
  ${(writeTreeAtPath "" tree)}
''
