# Overridden vterm to fetch a newer version
{ pkgs, ... }:

pkgs.emacsPackages.vterm.overrideAttrs(_: {
  src = pkgs.fetchFromGitHub{
    owner = "akermu";
    repo = "emacs-libvterm";
    rev = "58b4cc40ee9872a08fc5cbfee78ad0e195a3306c";
    sha256 = "1w5yfl8nq4k7xyldf0ivzv36vhz3dwdzk6q2vs3xwpx6ljy52px6";
  };
})
