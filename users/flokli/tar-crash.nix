{ ... }:

let
  customNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/445e6037e0c38daab0cb4649fde17b17ff8d840a.tar.gz";
    sha256 = "1i1ffagd91nd453ahanda57q432d5sv3a1ld4jj9p3xr72lq1wk6";
  };
  pkgs = import customNixpkgsSrc { };
in
pkgs.dockerTools.buildLayeredImage {
  name = "derp";
  contents = with pkgs;[
    dockerTools.fakeNss
    dockerTools.usrBinEnv
    bash
  ];

  fakeRootCommands = ''
    install -d -o nobody -g nogroup /work
  '';
  enableFakechroot = true;
  config = {
    User = "nobody";
    WorkingDir = "/work";
    Cmd = [ "/bin/bash" ];
  };
}
