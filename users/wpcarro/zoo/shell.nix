{ depot, ... }:

depot.users.wpcarro.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    servant-server
    aeson
    warp
    rio
  ];
}
