{ ... }:

let
  inherit (builtins)
    attrNames
    baseNameOf
    concatStringsSep
    filter
    hasAttr
    head
    isAttrs
    length
    listToAttrs
    map
    substring;

  assertMsg = pred: msg:
    if pred
    then true
    else builtins.trace msg false;

  argsWithPath = args: parts: args // {
    locatedAt = parts;
  };

  readDirVisible = path:
    let
      children = builtins.readDir path;
      shouldBeSkipped =
        if children ? ".skip-subtree"
        then { why = builtins.readFile (path + "/.skip-subtree"); }
        else { not-skipped = {}; };
      isVisible = f: (substring 0 1 f) != ".";
      names = filter isVisible (attrNames children);
      visible-children =
        listToAttrs (map (name: {
          inherit name;
          value = children.${name};
        }) names);
    in
      if visible-children ? "default.nix"
      then match shouldBeSkipped {
        not-skipped = {}:
          { with-default-nix = visible-children; };
        why = why:
          { skip-subdirs-default-nix = {
              inherit
                why
                visible-children;
            };
          };
      }
      else match shouldBeSkipped {
        not-skipped = {}:
          { no-default-nix = visible-children; };
        why = why:
          { skip-dir = why; };
      };

  # Create a mark containing the location of this attribute.
  marker = parts: {
    __readTree = parts;
  };

  # The marker is added to every set that was imported directly by
  # readTree.
  importWithMark = args: path: parts:
    let
      importedFile = import path;
      pathType = builtins.typeOf importedFile;
      imported =
        assert assertMsg
          (pathType == "lambda")
          "readTree: trying to import ${toString path}, but it’s a ${pathType}, you need to make it a function like { depot, pkgs, ... }";
        importedFile (argsWithPath args parts);
    in if (isAttrs imported)
      then imported // (marker parts)
      else imported;

  nixFileName = file:
    let res = builtins.match "(.*)\\.nix" file;
    in if res == null then null else head res;

  readTree = args: initPath: parts:
    let
      dir = readDirVisible initPath;
      self = importWithMark args initPath parts;
      joinChild = c: initPath + ("/" + c);

      # Import subdirectories of the current one, unless the special
      # `.skip-subtree` file exists which makes readTree ignore the
      # children.
      #
      # This file can optionally contain information on why the tree
      # should be ignored, but its content is not inspected by
      # readTree
      filterChildren = ch:
          let onlyDirectories = f: ch."${f}" == "directory"; in
          map (c: {
            name = c;
            value = readTree args (joinChild c) (parts ++ [ c ]);
          }) (filter onlyDirectories (attrNames ch));
      children = match dir {
        # TODO: remove trace, convert to readDir debbugging mode
        skip-dir = _why: builtins.trace "skipping dir ${initPath}: ${_why}" [];
        # TODO: remove trace, convert to readDir debugging mode
        skip-subdirs-default-nix = { why, visible-children }: builtins.trace "skipping subdirs of ${initPath}: ${why}" [];
        with-default-nix = filterChildren;
        no-default-nix = filterChildren;
       };

      # Import Nix files
      filterNixFiles = d: filter (f: f != null) (map nixFileName (attrNames d));
      nixFiles = match dir {
        skip-dir = _why: [];
        skip-subdirs-default-nix = { why, visible-children }: filterNixFiles visible-children;
        with-default-nix = filterNixFiles;
        no-default-nix = filterNixFiles;
      };
      nixChildren = map (c: let p = joinChild (c + ".nix"); in {
        name = c;
        value = importWithMark args p (parts ++ [ c ]);
      }) nixFiles;
    in if (dir ? with-default-nix || dir ? skip-subdirs-default-nix)
      then (if isAttrs self then self // (listToAttrs children) else self)
      else (listToAttrs (nixChildren ++ children) // (marker parts));

  # helpers

  match = sum: matcher:
    let cases = builtins.attrNames sum;
    in assert
      let len = builtins.length cases; in
        assertMsg (builtins.length cases == 1)
          ( "match: an instance of a sum is an attrset "
          + "with exactly one element, yours had ${toString len}"
          + ", namely: ${toString cases}" );
    let case = builtins.head cases;
    in assert
        assertMsg (matcher ? ${case})
        ( "match: \"${case}\" is not a valid case of this sum, "
        + "the matcher accepts: ${toString
            (builtins.attrNames matcher)}" );
    matcher.${case} sum.${case};


in {
   __functor = _: args: initPath: readTree args initPath [ (baseNameOf initPath) ];
}
