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

  /* Query the type of a path exposing the same information as would be by
     `builtins.readDir`, but for a single, specific target path.

     The information is returned as a tagged value, i. e. an attribute set with
     exactly one attribute where the type of the path is encoded in the name
     of the single attribute. The allowed tags and values are as follows:

     * `regular`: is a regular file, always `true` if returned
     * `directory`: is a directory, always `true` if returned
     * `missing`: path does not exist, always `true` if returned
     * `symlink`: path is a symlink, value is a string describing the type
       of its realpath which may be either:

       * `"directory"`: realpath of the symlink is a directory
       * `"regular-or-missing`": realpath of the symlink is either a regular
         file or does not exist. Due to limitations of the Nix expression
         language, we can't tell which.

     Type: path(-like) -> tag

     `tag` refers to the attribute set format of `//nix/tag`.

     Example:
       pathType ./foo.c
       => { regular = true; }

       pathType /home/lukas
       => { directory = true; }

       pathType ./result
       => { symlink = "directory"; }

       pathType ./link-to-file
       => { symlink = "regular-or-missing"; }

       pathType /does/not/exist
       => { missing = true; }

       # Check if a path exists
       !(pathType /file ? missing)

       # Check if a path is a directory or a symlink to a directory
       pathType /path ? directory || (pathType /path).symlink or null == "directory"

       # Match on the result using //nix/tag
       tag.match (nix.utils.pathType ./result) {
         symlink = v: "symlink to ${v}";
         directory  = _: "directory";
         regular = _: "regular";
         missing = _: "path does not exist";
       }
       => "symlink to directory"

       # Query path type
       (nix.tag.verifyTag (pathType /path)).name
       # or simply
       with builtins; head (attrNames (pathType /path))
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
      # Construct tag to use for the value
      thisPathType = containingDir.${builtins.baseNameOf path'} or "missing";
      # Trick to check if the symlink target exists and is a directory:
      # if we append a "/." to the string version of the path, Nix won't
      # canocalize it (which would strip any "/." in the path), so if
      # path' + "/." exists, we know that the symlink points to an existing
      # directory. If not, either the target doesn't exist or is a regular file.
      # TODO(sterni): is there a way to check reliably if the symlink target exists?
      isSymlinkDir = builtins.pathExists (path' + "/.");
    in {
      ${thisPathType} =
        /**/ if thisPathType != "symlink" then true
        else if isSymlinkDir              then "directory"
        else                                   "regular-or-missing";
    };

  pathType' = path:
    let
      p = pathType path;
    in
      if p ? missing
      then builtins.throw "${lib.generators.toPretty {} path} does not exist"
      else p;

  /* Check whether the given path is a directory.
     Throws if the path in question doesn't exist.

     Type: path(-like) -> bool
  */
  isDirectory = path: pathType' path ? directory;

  /* Checks whether the given path is a directory or
     a symlink to a directory. Throws if the path in
     question doesn't exist.

     Warning: Does not throw if the target file or
     directory doesn't exist, but the symlink does.

     Type: path(-like) -> bool
  */
  realPathIsDirectory = path: let
    pt = pathType' path;
  in pt ? directory || pt.symlink or null == "directory";

  /* Check whether the given path is a regular file.
     Throws if the path in question doesn't exist.

     Type: path(-like) -> bool
  */
  isRegularFile = path: pathType' path ? regular;

  /* Check whether the given path is a symbolic link.
     Throws if the path in question doesn't exist.

     Type: path(-like) -> bool
  */
  isSymlink = path: pathType' path ? symlink;

in {
  inherit
    drvTargets
    storePathName
    pathType
    isDirectory
    realPathIsDirectory
    isRegularFile
    isSymlink
    ;
}
