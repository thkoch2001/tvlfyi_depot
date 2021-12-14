# unix-opts is a portable command line argument parser
{ depot, pkgs, ...}:


let src = with pkgs; srcOnly lispPackages.unix-opts;
in depot.nix.buildLisp.library {
  name = "unix-opts";

  srcs = [
    "${src}/unix-opts.lisp"
  ];
}
