{ depot, pkgs, ... }:

let

  inherit (pkgs) runCommand;
  src = with pkgs; srcOnly lispPackages.quri;
  getSrc = f: src + "/src/" + f;

in depot.nix.buildLisp.library {
  name = "quri";

  deps = with depot.third_party.lisp; [
    babel
    alexandria
    split-sequence
    cl-utilities
    { sbcl = depot.nix.buildLisp.bundled "sb-cltl2"; }
    { sbcl = depot.nix.buildLisp.bundled "uiop"; }
  ];

  srcs = map getSrc [
    "util.lisp"
    "error.lisp"
    "port.lisp"
    "uri.lisp"
    "encode.lisp"
    "decode.lisp"

    # :module "uri-classes"
    "uri/ftp.lisp"
    "uri/http.lisp"
    "uri/ldap.lisp"
    "uri/file.lisp"
  ] ++ [(runCommand "etld.lisp" { } ''
    substitute ${src}/src/etld.lisp "$out" \
      --replace-fail \
        '#.(asdf:system-relative-pathname :quri #P"data/effective_tld_names.dat")' \
      '"${src}/data/effective_tld_names.dat"'
  '')]
  ++ map getSrc [
    "domain.lisp"
    "parser.lisp"
    "quri.lisp"
  ];
}
