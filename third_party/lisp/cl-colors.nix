{ depot, pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://github.com/tpapp/cl-colors.git";
    rev = "827410584553f5c717eec6182343b7605f707f75";
    hash = "sha256:0l446lday4hybsm9bq3jli97fvv8jb1d33abg79vbylpwjmf3y9a";
  };
in depot.nix.buildLisp.library {
  name = "cl-colors";
  deps = [
    depot.third_party.lisp.alexandria
    depot.third_party.lisp.let-plus
  ];
  srcs = [
    "${src}/package.lisp"
    "${src}/colors.lisp"
    "${src}/colornames.lisp"
    "${src}/hexcolors.lisp"
  ];
}
