{ depot, pkgs, ... }:

let
  inherit (pkgs) runCommand;
  inherit (depot.nix.buildLisp) bundled;

  src = pkgs.fetchFromGitHub {
    owner = "sabracrolleton";
    repo = "uax-15";
    rev = "a62fc7253663fac6467fc6b6bb69a73a0e8dfaa0";
    sha256 = "028kc3yfi29qjxs2nyr7dbrr2rcrr8lwpvbxvrq3g8bcwamc4jz4";
  };

  # src = ../../../uax-15;

in depot.nix.buildLisp.library {
  name = "uax-15";

  deps = with depot.third_party.lisp; [
    split-sequence
    cl-ppcre
    (bundled "asdf")
  ];

  srcs = [
    "${src}/uax-15.asd"
    "${src}/src/package.lisp"
    "${src}/src/utilities.lisp"
    "${src}/src/trivial-utf-16.lisp"
    (runCommand "precomputed-tables.lisp" {} ''
      substitute ${src}/src/precomputed-tables.lisp precomputed-tables.lisp \
        --replace "(asdf:system-source-directory (asdf:find-system 'uax-15 nil))" \
                  '"${src}/"'

      sed -i precomputed-tables.lisp \
        -e '17i(defvar *canonical-decomp-map*)' \
        -e '17i(defvar *compatible-decomp-map*)' \
        -e '17i(defvar *canonical-combining-class*)'

      cp precomputed-tables.lisp $out
    '')
    "${src}/src/normalize-backend.lisp"
    "${src}/src/uax-15.lisp"
  ];
}
