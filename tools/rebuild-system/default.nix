{ depot, pkgs, lib, ... }:

let
  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    optparse-applicative
  ]);

  rebuildSystem = pkgs.stdenv.mkDerivation {
    name = "rebuild-system";
    src = ./.;
    buildPhase = "${ghc}/bin/ghc Main.hs";
    installPhase = "mv ./Main $out";
  };
in pkgs.writeShellScript "rebuild-system" ''
  PATH=${lib.strings.makeBinPath (with pkgs; [ nix git ])}
  ${rebuildSystem} "$@"
''
