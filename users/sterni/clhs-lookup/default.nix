{ pkgs, depot, ... }:

let
  inherit (pkgs) fetchzip writeText;
  inherit (depot.nix) buildLisp;
  inherit (builtins) replaceStrings;

  clhsVersion = "7-0";

  clhs = fetchzip {
    name = "HyperSpec-${replaceStrings [ "-" ] [ "." ] clhsVersion}";
    url = "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-${clhsVersion}.tar.gz";
    sha256 = "1zsi35245m5sfb862ibzy0pzlph48wvlggnqanymhgqkpa1v20ak";
    stripRoot = false;
  };

  clhs-path = writeText "clhs-path.lisp" ''
    (in-package :clhs-lookup.clhs-path)
    (defparameter *clhs-path* (pathname "${clhs}/"))
  '';

  clhs-lookup = buildLisp.program {
    name = "clhs-lookup";

    deps = [
      {
        default = buildLisp.bundled "asdf";
        sbcl = buildLisp.bundled "uiop";
      }
    ];

    srcs = [
      ./packages.lisp
      clhs-path
      ./clhs-lookup.lisp
    ];
  };
in
clhs-lookup
