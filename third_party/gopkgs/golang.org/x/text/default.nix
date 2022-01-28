{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "golang.org/x/text";
    src =
      pkgs.fetchgit
        {
          url = "https://go.googlesource.com/text";
          rev = "cbf43d21aaebfdfeb81d91a5f444d13a3046e686";
          hash = "sha256:1h6z2x4ijzd1126zk3lf8f3bp98j1irs7xg6p8nwpymkqkw5laq8";
        };
  }
