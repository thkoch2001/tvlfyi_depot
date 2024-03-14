{ depot, pkgs, ... }:

let
  src = pkgs.applyPatches {
    src = pkgs.fetchFromGitHub {
      owner = "chaitanyagupta";
      repo = "qbase64";
      rev = "4ac193ed6b35a867ca453ed74acc128c9a077407";
      sha256 = "06daqqfdd51wkx0pyxgz7zq4ibzsqsgn3qs04jabx67gyybgnmjm";
    };

    patches = [
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

    srcs = getSrcs [ "qbase64-test.lisp" ];

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
