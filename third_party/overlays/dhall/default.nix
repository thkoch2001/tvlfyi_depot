{ ... }:

self: super:

let
  dhall-source = subdir: pkg: super.haskell.lib.overrideSrc pkg {
    src = "${super.fetchFromGitHub {
      owner = "Profpatsch";
      repo = "dhall-haskell";
      rev = "fix-dhall-to-nix-key-encoding";
      sha256 = "sha256-P4MmQfyQ7MC5Jbs1o/+AJX/kdps2CjeOrtdpK85Tg68=";
    }}/${subdir}";
  };

  # binary releases of dhall tools, since the build in nixpkgs is
  # broken most of the time. The binaries are also fully static
  # builds, instead of the half-static crap that nixpkgs produces.
  easy-dhall-nix =
    import
      (builtins.fetchTarball {
        url = "https://github.com/justinwoo/easy-dhall-nix/archive/eae7f64c4d6c70681e5a56c84198236930ba425e.tar.gz";
        sha256 = "1y2x15v8a679vlpxazjpibfwajp6zph60f8wjcm4xflbvazk0dx7";
      })
      { inherit self; };
in
{
  # TODO: this is to fix a bug in dhall-nix
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      dhall = dhall-source "dhall" hsSuper.dhall;
      dhall-nix = dhall-source "dhall-nix" hsSuper.dhall-nix;
    };
  };
  # dhall = easy-dhall-nix.dhall-simple;
  # dhall-nix = easy-dhall-nix.dhall-nix-simple;
  dhall-bash = easy-dhall-nix.dhall-bash-simple;
  dhall-docs = easy-dhall-nix.dhall-docs-simple;
  dhall-json = easy-dhall-nix.dhall-json-simple;
  dhall-lsp-server = easy-dhall-nix.dhall-lsp-simple;
  # not yet in dhall-simple
  # dhall-nixpkgs = easy-dhall-nix.dhall-nixpkgs-simple;
  dhall-yaml = easy-dhall-nix.dhall-yaml-simple;
}
