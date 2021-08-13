# Usocket is a portable socket library
{ depot, pkgs, ... }:

let
  inherit (depot.nix) buildLisp;

  src = pkgs.fetchFromGitHub {
    owner = "usocket";
    repo = "usocket";
    rev = "fdf4fd1e0051ce83340ccfbbc8a43a462bb19cf2";
    sha256 = "0x746wr2324l6bn7skqzgkzcbj5kd0zp2ck0c8rldrw0rzabg826";
  };
in buildLisp.library {
  name = "usocket";
  deps = with depot.third_party.lisp; [
    (buildLisp.bundled "asdf")
    {
      ecl = buildLisp.bundled "sb-bsd-sockets";
      sbcl = buildLisp.bundled "sb-bsd-sockets";
    }
    split-sequence
  ];

  srcs = [
    # usocket also reads its version from ASDF, but there's further
    # shenanigans happening there that I don't intend to support right
    # now. Behold:
    (builtins.toFile "usocket.asd" ''
      (in-package :asdf)
      (defsystem usocket
        :version "0.8.3")
    '')
  ] ++
  # Now for the regularly scheduled programming:
  (map (f: src + ("/" + f)) [
    "package.lisp"
    "usocket.lisp"
    "condition.lisp"
  ] ++ [
    { sbcl = "${src}/backend/sbcl.lisp"; }

    # ECL actually has two files, it supports the SBCL backend,
    # but usocket also has some ECL specific code
    { ecl = "${src}/backend/sbcl.lisp"; }
    { ecl = "${src}/backend/ecl.lisp"; }

    # Same for CCL
    { ccl = "${src}/backend/openmcl.lisp"; }
    { ccl = "${src}/backend/clozure.lisp"; }
  ]);
}
