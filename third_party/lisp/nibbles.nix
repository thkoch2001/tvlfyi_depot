{ depot, pkgs, ... }:

let
  inherit (depot.nix.buildLisp) bundled;
  src = pkgs.fetchFromGitHub {
    owner = "froydnj";
    repo = "nibbles";
    rev = "9de8c755c2ff24117748a3271e8582bb8d4a6b6c";
    sha256 = "11rznn33m950mp4zgnpyjaliy3z3rvibfdr8y4vnk2aq42kqi7dj";
  };

in depot.nix.buildLisp.library {
  name = "nibbles";

  deps = with depot.third_party.lisp; [
    (bundled "asdf")
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "types.lisp"
    "macro-utils.lisp"
    "types.lisp"
    "vectors.lisp"
    "streams.lisp"
  ];

  badImplementations = [
    "ccl" # duplicate type definitions
  ];
}
