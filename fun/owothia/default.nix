{ depot ? (import ../../../. { })
, pkgs ? depot.third_party.nixpkgs
, ...
}:

let
  basePkg = pkgs.haskellPackages.callPackage ./pkg.nix { };
in

pkgs.haskell.lib.overrideSrc basePkg {
  src = depot.third_party.gitignoreSource ./.;
  version = "canon";
}
