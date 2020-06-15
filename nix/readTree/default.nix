{ ... }:

args: initPath:

let
  inherit (builtins)
    attrNames
    baseNameOf
    filter
    hasAttr
    head
    isAttrs
    length
    listToAttrs
    map
    match
    readDir
    substring;

  argsWithPath = parts: args // {
    locatedAt = parts;
  };

  readDirVisible = path:
    let
      children = readDir path;
      isVisible = f: f == ".skip-subtree" || (substring 0 1 f) != ".";
      names = filter isVisible (attrNames children);
    in listToAttrs (map (name: {
      inherit name;
      value = children.${name};
    }) names);

  isDerivation = x: isAttrs x && x ? "type" && x.type == "derivation";

  # Merges rhs into lhs. If lhs is a derivation that supports
  # passthru, then values will be added to the passthru set to avoid
  # unnecessary cache-busts.
  mergeWithPassthru = lhs: rhs:
    if (isDerivation lhs && hasAttr "passthru" lhs)
    then lhs.overrideAttrs(old: {
      passthru = if hasAttr "passthru" old
                 then old.passthru // rhs
                 else rhs;
    })
    else lhs // rhs;

  # The marker is added to every set that was imported directly by
  # readTree.
  importWithMark = path: parts:
    let imported = import path (argsWithPath parts);
    in if (isAttrs imported)
      then mergeWithPassthru imported { __readTree = true; }
      else imported;

  nixFileName = file:
    let res = match "(.*)\.nix" file;
    in if res == null then null else head res;

  readTree = path: parts:
    let
      dir = readDirVisible path;
      self = importWithMark path parts;
      joinChild = c: path + ("/" + c);

      # Import subdirectories of the current one, unless the special
      # `.skip-subtree` file exists which makes readTree ignore the
      # children.
      #
      # This file can optionally contain information on why the tree
      # should be ignored, but its content is not inspected by
      # readTree
      filterDir = f: dir."${f}" == "directory";
      children = if hasAttr ".skip-subtree" dir then [] else map (c: {
        name = c;
        value = readTree (joinChild c) (parts ++ [ c ]);
      }) (filter filterDir (attrNames dir));

      # Import Nix files
      nixFiles = filter (f: f != null) (map nixFileName (attrNames dir));
      nixChildren = map (c: let p = joinChild (c + ".nix"); in {
        name = c;
        value = importWithMark p (parts ++ [ c ]);
      }) nixFiles;
    in if dir ? "default.nix"
      then (if isAttrs self then mergeWithPassthru self (listToAttrs children)
                            else self)
      else listToAttrs (nixChildren ++ children);
in readTree initPath [ (baseNameOf initPath) ]
