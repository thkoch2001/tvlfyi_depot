# Babel is an encoding conversion library for Common Lisp.
{ depot, ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/cl-babel/babel.git";
    rev = "f892d0587c7f3a1e6c0899425921b48008c29ee3"; # 2020-07-19
  };
in depot.nix.buildLisp.library {
  name = "babel";
  deps = [
    depot.third_party.lisp.alexandria
    depot.third_party.lisp.trivial-features
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "packages.lisp"
    "encodings.lisp"
    "enc-ascii.lisp"
    "enc-ebcdic.lisp"
    "enc-ebcdic-int.lisp"
    "enc-iso-8859.lisp"
    "enc-unicode.lisp"
    "enc-cp1251.lisp"
    "enc-cp1252.lisp"
    "jpn-table.lisp"
    "enc-jpn.lisp"
    "enc-gbk.lisp"
    "enc-koi8.lisp"
    "external-format.lisp"
    "strings.lisp"
    "gbk-map.lisp"
    "sharp-backslash.lisp"
  ];
}
