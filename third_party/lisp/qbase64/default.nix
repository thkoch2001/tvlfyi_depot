{ depot, pkgs, ... }:

let
  src = pkgs.applyPatches {
    src = pkgs.fetchFromGitHub {
      owner = "chaitanyagupta";
      repo = "qbase64";
      rev = "0.3.0";
      sha256 = "1dir0s70ca3hagxv9x15zq4p4ajgl7jrcgqsza2n2y7iqbxh0dwi";
    };

    patches = [
      (pkgs.fetchpatch {
        name = "qbase64-cl-fad.patch";
        url = "https://github.com/chaitanyagupta/qbase64/commit/daaa31a6d601c795b5d5037d3099430051ed4dd4.patch";
        sha256 = "0f8lmsd2g7jkf04yrkd9p160f18qkz0pr2bks0x6m7z8qw2dmjv4";
      })
      # qbase64 expects macOS base64
      ./coreutils-base64.patch
    ];
  };

  getSrcs = builtins.map (p: "${src}/${p}");

in

depot.nix.buildLisp.library {
  name = "qbase64";

  srcs = getSrcs [
    "package.lisp"
    "utils.lisp"
    "stream-utils.lisp"
    "qbase64.lisp"
  ];

  deps = [
    depot.third_party.lisp.trivial-gray-streams
    depot.third_party.lisp.metabang-bind
  ];

  tests = {
    name = "qbase64-tests";

    srcs = getSrcs [
      "qbase64-test.lisp"
    ];

    deps = [
      {
        sbcl = depot.nix.buildLisp.bundled "uiop";
        default = depot.nix.buildLisp.bundled "asdf";
      }
      depot.third_party.lisp.fiveam
      depot.third_party.lisp.cl-fad
    ];

    expression = ''
      (fiveam:run! '(qbase64-test::encoder 'qbase64-test::decoder))
    '';
  };
}
