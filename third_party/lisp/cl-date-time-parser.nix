{ depot
, pkgs
, ...
}:
depot.nix.buildLisp.library
  {
    name = "cl-date-time-parser";
    srcs = [
      (
        pkgs.fetchurl
          {
            url =
              "https://raw.githubusercontent.com/tkych/cl-date-time-parser/00d6fc70b599f460fdf13cf0cf7e6bf843312410/date-time-parser.lisp";
            sha256 = "0zrkv1q3sx5ksijxhw45ixf1hy5b9biii6i6v41h12q6pbkfqz69";
          }
      )
    ];
    deps = [
      depot.third_party.lisp.alexandria
      depot.third_party.lisp.anaphora
      depot.third_party.lisp.split-sequence
      depot.third_party.lisp.cl-ppcre
      depot.third_party.lisp.local-time
      depot.third_party.lisp.parse-float
    ];
  }
