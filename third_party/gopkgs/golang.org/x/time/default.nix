{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "golang.org/x/time";
    src =
      pkgs.fetchgit
        {
          url = "https://go.googlesource.com/time";
          rev = "555d28b269f0569763d25dbe1a237ae74c6bcc82";
          hash = "sha256:1rhl4lyz030kwfsg63yk83yd3ivryv1afmzdz9sxbhcj84ym6h4r";
        };
  }
