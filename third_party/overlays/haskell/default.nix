# Defines an overlay for overriding Haskell packages, for example to
# avoid breakage currently present in nixpkgs or to modify package
# versions.

{ lib, ... }:

self: super: # overlay parameters for the nixpkgs overlay

let
  overrides = hsSelf: hsSuper: with super.haskell.lib; {
    generic-arbitrary = appendPatch hsSuper.generic-arbitrary
      [ ./patches/generic-arbitrary-export-garbitrary.patch ];

    error = lib.pipe {} [
      (_: hsSelf.callCabal2nix "error" (builtins.fetchTarball {
        url = "https://github.com/Profpatsch/error/archive/v0.2.1.0.tar.gz";
        sha256 = "0fqglyjlrh3gqh2vskp6yyqdxhv0b1k7si5ai1mqyjznxn0c7kwk";
      }) {})
      dontCheck
    ];

    nix-diff = lib.pipe hsSuper.nix-diff [
      (drv: overrideSrc drv {
        src = builtins.fetchTarball {
          # https://github.com/Profpatsch/nix-diff/commit/ee5bd082d2d780b7ff0a82ae7ae6dd73aafb68be
          url = "https://github.com/Profpatsch/nix-diff/archive/ee5bd082d2d780b7ff0a82ae7ae6dd73aafb68be.tar.gz";
          sha256 = "10x8551ic03hqxav5ngcyxngpfwjs0l3ygyxh6gn42n43w55ax6g";
        };
      })
      (drv: addBuildDepend drv hsSelf.error)
    ];

  };
in {
  haskellPackages = super.haskellPackages.override {
    inherit overrides;
  };
}
