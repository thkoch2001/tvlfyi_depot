{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "golang.org/x/sys";
    src =
      pkgs.fetchgit
        {
          url = "https://go.googlesource.com/sys";
          rev = "ac6580df4449443a05718fd7858c1f91ad5f8d20";
          hash = "sha256:14gvx65w5lddi20s4wypbbvbg9ni3m8777jhp9nqxhixc61k3dyi";
        };
  }
