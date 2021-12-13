let
  briefcase = import <briefcase> {};
in briefcase.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    quickcheck-simple
    checkers
  ];
}
