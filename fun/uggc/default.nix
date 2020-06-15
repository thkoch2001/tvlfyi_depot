{ depot, pkgs, ... }@args:

let
  inherit (pkgs) gopkgs;
  uggc = depot.nix.buildGo.program {
    name = "uggc";
    srcs = [
      ./main.go
    ];
    deps = [
      gopkgs."github.com".pkg.browser.gopkg
    ];
  };
in uggc.overrideAttrs(old: {
  buildCommand = old.buildCommand + ''
    install -D ${./uggc.desktop} $out/share/applications/uggc.desktop
    sed "s|@out@|$out|g" -i $out/share/applications/uggc.desktop
  '';
})
