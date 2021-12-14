{ depot, ... }:

depot.users.wpcarro.buildHaskell.program {
  name = "server";
  srcs = builtins.path {
    path = ./.;
    name = "LearnPianoChords-server-src";
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
    wai-cors
    warp
    jwt
    unordered-containers
    base64
    http-conduit
    rio
    envy
    req
  ];
}
