let
  pkgs = import /home/wpcarro/nixpkgs {};
  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
    hpkgs.servant-server
    hpkgs.aeson
    hpkgs.wai-cors
    hpkgs.warp
    hpkgs.jwt
    hpkgs.unordered-containers
    hpkgs.base64
    hpkgs.http-conduit
    hpkgs.rio
  ]);
in pkgs.stdenv.mkDerivation {
  name = "LearnPianoChords-server";
  srcs = builtins.path {
    path = ./.;
    name = "LearnPianoChords-server-src";
  };
  buildPhase = ''
    ${ghc} -O Main.hs \
      -XOverloadedStrings \
      -XNoImplicitPrelude \
      -XRecordWildCards \
      -XTypeApplications
  '';
}
