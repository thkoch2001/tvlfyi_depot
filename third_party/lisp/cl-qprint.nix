{ depot, pkgs, ... }:

let
  name = "cl-qprint";
  src = pkgs.fetchFromGitHub {
    owner = "eugeneia";
    repo = "cl-qprint";
    rev = "bfe398551cbfb7ca84a9ba59a26a1116ac5c06eb"; # 2015-07-26
    sha256 = "099h0rrdzxnlmn8avi72mg2dl0kccp7w01b2p9nwyy4b8yr32cir";
  };

  getSrcs = builtins.map (p: "${src}/${p}");
in

depot.nix.buildLisp.library {
  inherit name;

  srcs = getSrcs [
    "cl-qprint.asd"
    "base.lisp"
  ];

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
    depot.third_party.lisp.flexi-streams
  ];

  tests = {
    name = "cl-qprint-test";
    expression = "(progn (cl-qprint-test:test-cl-qprint) t)"; # uses asserts
    srcs = getSrcs [ "test.lisp" ];
  };
}
