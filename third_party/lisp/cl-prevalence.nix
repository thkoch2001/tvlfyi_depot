# cl-prevalence is an implementation of object prevalence for CL (i.e.
# an in-memory database)
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "40ants";
    repo = "cl-prevalence";
    rev = "b1f90a525f37be0335a8761051fa5661aa74b696";
    sha256 = "1svw58pp7jxb9l08cgnqxf0cf8qa9qsb0z2fnv86a51z7pfz4c0g";
  };
in depot.nix.buildLisp.library {
  name = "cl-prevalence";

  deps = with depot.third_party.lisp; [ bordeaux-threads s-xml s-sysdeps ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "serialization/serialization.lisp"
    "serialization/xml.lisp"
    "serialization/sexp.lisp"
    "prevalence.lisp"
    "managed-prevalence.lisp"
    "master-slave.lisp"
    "blob.lisp"
  ];
}
