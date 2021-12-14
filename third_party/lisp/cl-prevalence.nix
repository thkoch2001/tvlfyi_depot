# cl-prevalence is an implementation of object prevalence for CL (i.e.
# an in-memory database)
{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.cl-prevalence;
in depot.nix.buildLisp.library {
  name = "cl-prevalence";

  deps = with depot.third_party.lisp; [
    bordeaux-threads
    s-xml
    s-sysdeps
  ];

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
