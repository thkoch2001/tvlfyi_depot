{ depot, pkgs, ... }:

let
  src = pkgs.applyPatches {
    name = "mime4cl-source";
    src = pkgs.fetchzip {
      url = "http://wcp.sdf-eu.org/software/mime4cl-20150207T211851.tbz";
      sha256 = "0xvg9zywmksvw31zcra0mdac7qfmxkn81d6c1fi2avmkxwcy4zc7";
    };

    patches = [
      # Fix stream-file-position passing NIL as the second argument
      # to file-position which SBCL at least doesn't like.
      ./stream-file-position-optional-arg.patch
    ];
  };

  getSrcs = builtins.map (p: "${src}/${p}");
in

depot.nix.buildLisp.library {
  name = "mime4cl";

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
    depot.third_party.lisp.sclf
    depot.third_party.lisp.npg
  ];

  srcs = getSrcs [
    "mime4cl.asd"
    "package.lisp"
    "endec.lisp"
    "streams.lisp"
    "mime.lisp"
    "address.lisp"
  ];

  tests = {
    name = "mime4cl-tests";

    srcs = getSrcs [
      "mime4cl-tests.asd"
      "test/rt.lisp"
      "test/package.lisp"
      "test/endec.lisp"
      "test/address.lisp"
      "test/mime.lisp"
    ];

    expression = "(rtest:do-tests)";
  };
}
