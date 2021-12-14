{ depot, ... }:

depot.users.wpcarro.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    time
    aeson
    either
    hspec
  ];
}
