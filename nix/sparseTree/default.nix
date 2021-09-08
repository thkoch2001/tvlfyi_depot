# Build a “sparse” version of a given directory, only including contained files
# and directories if they are listed in a supplied list:
#
# # A very minimal depot
# sparseTree ./depot [
#   ./default.nix
#   ./depot/nix/readTree/default.nix
#   ./third_party/nixpkgs
#   ./third_party/overlays
# ]
{ pkgs, lib, ... }:

# root path to use as a reference point
root:
# list of paths below `root` that should be
# included in the resulting directory
paths:

let
  rootLength = builtins.stringLength (toString root);

  # Count slashes in a path.
  #
  # Type: path -> int
  depth = path: lib.pipe path [
    toString
    (builtins.split "/")
    (builtins.filter builtins.isList)
    builtins.length
  ];

  # (Parent) directories will be created from deepest to shallowest
  # which should mean no conflicts are caused unless both a child
  # and its parent directory are in the list of paths.
  # TODO(sterni): improve error messages in such cases
  fromDeepest = lib.sort (a: b: depth a < depth b) paths;

  # Create a set which contains the source path to copy / symlink and
  # it's destination, so the path below the destination root including
  # a leading slash. Additionally some sanity checking is done.
  makeSymlink = path:
    let
      strPath = toString path;
      contextPath = "${path}";
      belowRoot = builtins.substring rootLength (-1) strPath;
      prefix = builtins.substring 0 rootLength strPath;
    in assert toString root == prefix; {
      src = contextPath;
      dst = belowRoot;
    };

  symlinks = builtins.map makeSymlink fromDeepest;
in

# TODO(sterni): teach readTree to also read symlinked directories,
# so we ln -sT instead of cp -aT.
pkgs.runCommandNoCC "sparse-${builtins.baseNameOf root}" {} (
  lib.concatMapStrings ({ src, dst }: ''
    mkdir -p "$(dirname "$out${dst}")"
    cp -aT --reflink=auto "${src}" "$out${dst}"
  '') symlinks
)
