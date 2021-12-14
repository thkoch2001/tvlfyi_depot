{ depot, ... }:

depot.users.wpcarro.buildHaskell.program {
  name = "zoo";
  srcs = builtins.path {
    path = ./.;
    name = "zoo-src";
  };
  ghcExtensions = [
    "OverloadedStrings"
    "NoImplicitPrelude"
    "RecordWildCards"
    "TypeApplications"
  ];
  deps = hpkgs: with hpkgs; [
    servant-server
    aeson
    warp
    rio
  ];
}
