{ pkgs, ... }:

# `fs` contains utility functions for working with the filesystem.

let
  inherit (builtins) attrNames hasAttr map readDir;
  inherit (pkgs.lib) filterAttrs;
in {
  # Returns a list of all of the regular files in `dir`.
  files = dir:
    map (name: dir + "/${name}")
      (attrNames
        (filterAttrs (_: type: type == "regular") (readDir dir)));

  # Returns a list of all of the directories in `dir`.
  dirs = dir:
    map (name: dir + "/${name}")
      (attrNames
        (filterAttrs (_: type: type == "directory") (readDir dir)));

  # Returns a list of paths to all of the `name` files starting at `dir`.
  find = name: dir:
    if hasAttr name (readDir dir) then
      [ (dir + name) ] ++ concatMap findAllDefaultNix (dirs dir)
    else
      concatMap findAllDefaultNix (dirs dir);

  # Looks for `name` in `dir`; if it cannot find it, it checks the parent
  # directory.
  resolve = name: dir:
    if hasAttr name (readDir dir) then
      dir + "/${name}"
    else
      # This prevents the function from infinitely recursing and eventually
      # stack overflowing.
      if (dirOf dir) == dir then
        null
      else
        resolve name (dirOf dir);
  };
}
