let
  briefcase = import /home/wpcarro/briefcase {};
in briefcase.buildHaskell.program {
  name = "shift-time";
  srcs = builtins.path {
    path = ./.;
    name = "shift-time-src";
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
