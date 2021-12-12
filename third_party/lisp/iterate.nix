# iterate is an iteration construct for Common Lisp, similar to the
# LOOP macro.
{ depot, ... }:

let
  src = builtins.fetchGit {
    url = "https://gitlab.common-lisp.net/iterate/iterate.git";
    rev = "c24f6715bb3b962499bb4643636baaac2df4a957"; # 2021-05-23, 1.5.3
  };
in depot.nix.buildLisp.library {
  name = "iterate";
  srcs = [ "${src}/package.lisp" "${src}/iterate.lisp" ];
}
