{ depot, ... }:

depot.users.wpcarro.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    quickcheck-simple
    checkers
  ];
}
