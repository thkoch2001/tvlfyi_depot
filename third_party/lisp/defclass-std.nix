{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
    owner = "EuAndreh";
    repo = "defclass-std";
    rev = "0300f171c1308e5ff3efd66b4f4e766f2bcde259";
    sha256 = "0ggzh80ajx4k6w5c3xprnd7m27q5hx9xx9lxs4jv0pbrlg18ijcw";
  };
in depot.nix.buildLisp.library {
  name = "defclass-std";
  deps = with depot.third_party.lisp; [
    alexandria
    anaphora
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "defclass-std.lisp"
  ];
}
