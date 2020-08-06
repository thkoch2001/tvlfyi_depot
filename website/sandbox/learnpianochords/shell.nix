let
  pkgs = import /home/wpcarro/nixpkgs {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-live
    (haskellPackages.ghcWithPackages (hpkgs: [
      hpkgs.hspec
      hpkgs.servant-server
      hpkgs.aeson
      hpkgs.wai-cors
      hpkgs.warp
    ]))
  ];
}
