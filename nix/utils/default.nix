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
        # strip leading storeDir and trailing slashes
        noStoreDir = lib.removeSuffix "/"
          (lib.removePrefix "${builtins.storeDir}/" p);
        # a basename of a child of a store path isn't really
        # referring to a store path, so removing the string
        # context is safe (e. g. "hello" for "${hello}/bin/hello").
        basename = builtins.unsafeDiscardStringContext
          (builtins.baseNameOf p);
      in
        # If p is a direct child of storeDir, we need to remove
        # the leading hash as well to make sure that:
        # `storePathName drv == storePathName (toString drv)`.
        if noStoreDir == basename
        then builtins.substring 33 (-1) basename
        else basename
    else builtins.throw "Don't know how to get (base)name of "
      + lib.generators.toPretty {} p;

  /* Get the type of a path itself as it would be returned for a
     directory child by builtins.readDir.

     Type: path(-like) -> option<string>

     Example:
       pathType ./foo.c
       => "regular"

       pathType /home/lukas
       => "directory"

       pathType ./result
       => "symlink"

       pathType /does/not/exist
       => null
  */
  pathType = path:
    let
      # baseNameOf is very annoyed if we proceed with string context.
      # We need to call toString to prevent unsafeDiscardStringContext
      # from importing a path into store which messes with base- and
      # dirname of course.
      path'= builtins.unsafeDiscardStringContext (toString path);
      # To read the containing directory we absolutely need
      # to keep the string context, otherwise a derivation
      # would not be realized before our check (at eval time)
      containingDir = builtins.readDir (builtins.dirOf path);
    in
      containingDir.${builtins.baseNameOf path'} or null;

  pathType' = path:
    let
      p = pathType path;
    in
      if p == null
      then builtins.throw "${lib.generators.toPretty {} path} does not exist"
      else p;

  /* Check whether the given path exists.

     Type: path(-like) -> bool
  */
  pathExists = path: ! isNull (pathType path);

  /* Check whether the given path is a directory.
     Throws if the path in question doesn't exist.

     Type: path(-like) -> bool
  */
  isDirectory = path: pathType' path == "directory";

  /* Check whether the given path is a regular file.
     Throws if the path in question doesn't exist.

     Type: path(-like) -> bool
  */
  isRegularFile = path: pathType' path == "regular";

  /* Check whether the given path is a symbolic link.
     Throws if the path in question doesn't exist.

     Type: path(-like) -> bool
  */
  isSymlink = path: pathType' path == "symlink";

in {
  inherit
    drvTargets
    storePathName
    pathType
    pathExists
    isDirectory
    isRegularFile
    isSymlink
    ;
}
