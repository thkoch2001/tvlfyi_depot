let
  briefcase = import <briefcase> {};
in briefcase.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    servant-server
    aeson
    warp
    rio
  ];
}
