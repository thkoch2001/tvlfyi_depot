{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "cloud.google.com/go";
    src =
      pkgs.fetchgit
        {
          url = "https://code.googlesource.com/gocloud";
          rev = "4f03f8e4ba168c636e1c218da7ab41a1c8c0d8cf";
          hash = "sha256:1cgr8f9349r4ymp2k0x49lby47jgi40bblvl1dk6i99ij6faj93d";
        };
  }
