{ depot, ... }@args:

let
  gopkgs = depot.third_party.gopkgs;
in
depot.nix.buildGo.program {
  name = "uggc";
  srcs = [
    ./main.go
  ];
  deps = [
    gopkgs."github.com".pkg.browser.gopkg
  ];
}
