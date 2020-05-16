# EXWM straight from GitHub. As of 2020-05-15, XELB in nixpkgs is
# already at a recent enough version and does not need to be
# overridden.
{ pkgs, ... }:

pkgs.emacsPackages.exwm.overrideAttrs(_: {
  src = pkgs.fetchFromGitHub {
    owner = "ch11ng";
    repo = "exwm";
    rev = "48db94f48bea1137132345abfe8256cfc6219248";
    sha256 = "0jj12z6m5kvanq19gds3jpvid2mg8w28bbbq9iycl751y2sj4l1r";
  };
})
