{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "lmj";
    repo = "global-vars";
    rev = "c749f32c9b606a1457daa47d59630708ac0c266e";
    sha256 = "06m3xc8l3pgsapl8fvsi9wf6y46zs75cp9zn7zh6dc65v4s5wz3d";
  };

in depot.nix.buildLisp.library {
  name = "global-vars";
  srcs = [ "${src}/global-vars.lisp" ];
}
