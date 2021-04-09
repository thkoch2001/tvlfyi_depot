# Use the upstream git derivation (there's a lot of stuff happening in
# there!) and just override the source:
{ pkgs, ... }:

(pkgs.git.overrideAttrs(_: {
  version = "2.29.2";
  src = ./.;
  doInstallCheck = false;
  preConfigure = ''
    ${autoconf}/bin/autoreconf -i
  '';
})).override {
  sendEmailSupport = true;
}
