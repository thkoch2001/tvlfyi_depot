let
  briefcase = import /home/wpcarro/briefcase {};
in briefcase.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    hspec
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
  ];
}
