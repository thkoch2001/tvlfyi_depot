let
  pkgs = import /home/wpcarro/nixpkgs {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: [
      hpkgs.hspec
      hpkgs.servant-server
      hpkgs.aeson
      hpkgs.wai-cors
      hpkgs.warp
      hpkgs.jwt
      hpkgs.unordered-containers
      hpkgs.base64
      hpkgs.http-conduit
      hpkgs.rio
      hpkgs.envy
    ]))
  ];
}
