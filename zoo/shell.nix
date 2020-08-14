let
  briefcase = import /home/wpcarro/briefcase {};
in briefcase.buildHaskell.shell {
  deps = hpkgs: with hpkgs; [
    servant-server
    aeson
    warp
    rio
  ];
}
