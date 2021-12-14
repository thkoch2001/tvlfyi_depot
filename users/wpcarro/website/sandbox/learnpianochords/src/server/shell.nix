{ depot, ... }:

depot.users.wpcarro.buildHaskell.shell {
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
    req
  ];
}
