{ depot, ... }:

depot.users.wpcarro.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    hspec
    unordered-containers
  ];
}
