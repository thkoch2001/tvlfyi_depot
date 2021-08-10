{ depot, pkgs, ... }:

let
  inherit (pkgs) sbcl runCommand writeText;
  inherit (depot.nix.buildLisp) bundled;

  src = pkgs.fetchFromGitHub {
    owner = "edicl";
    repo = "cl-unicode";
    rev = "8073fc5634c9d4802888ac03abf11dfe383e16fa";
    sha256 = "0ykx2s9lqfl74p1px0ik3l2izd1fc9jd1b4ra68s5x34rvjy0hza";
  };

  cl-unicode-base = depot.nix.buildLisp.library {
    name = "cl-unicode-base";
    deps = with depot.third_party.lisp; [
      cl-ppcre
    ];

    srcs = map (f: src + ("/" + f)) [
      "packages.lisp"
      "specials.lisp"
      "util.lisp"
    ];
  };

  cl-unicode-build = depot.nix.buildLisp.program {
    name = "cl-unicode-build";
    deps = with depot.third_party.lisp; [
      cl-unicode-base
      flexi-streams
      (bundled "asdf")
    ];

    srcs = (map (f: src + ("/build/" + f)) [
      "util.lisp"
      "char-info.lisp"
      "read.lisp"
    ]) ++ [
      (runCommand "dump.lisp" {} ''
        substitute ${src}/build/dump.lisp $out \
          --replace ':defaults *this-file*' ":defaults (uiop:getcwd)"
      '')

      (writeText "export-create-source-files.lisp" ''
        (in-package :cl-unicode)
        (export 'create-source-files)
      '')
    ];

    main = "cl-unicode:create-source-files";
  };


  generated = runCommand "cl-unicode-generated" {} ''
    mkdir -p $out/build
    mkdir -p $out/test
    cd $out/build
    pwd
    ${cl-unicode-build}/bin/cl-unicode-build
  '';

in
depot.nix.buildLisp.library {
  name = "cl-unicode";
  deps = [cl-unicode-base];
  srcs = [
    "${src}/conditions.lisp"
    "${generated}/lists.lisp"
    "${generated}/hash-tables.lisp"
    "${src}/api.lisp"
    "${generated}/methods.lisp"
    "${src}/test-functions.lisp"
    "${src}/derived.lisp"
    "${src}/alias.lisp"
  ];
}
