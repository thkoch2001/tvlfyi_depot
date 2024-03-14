{ ... }:

self: super:

let

  # binary releases of dhall tools, since the build in nixpkgs is
  # broken most of the time. The binaries are also fully static
  # builds, instead of the half-static crap that nixpkgs produces.
  easy-dhall-nix = import (builtins.fetchTarball {
    url = "https://github.com/justinwoo/easy-dhall-nix/archive/dce9acbb99776a7f1344db4751d6080380f76f57.tar.gz";
    sha256 = "0ckp6515gfvbxm08yyll87d9vg8sq2l21gwav2npzvwc3xz2lccf";
  }) { pkgs = self; };
in
{
  # ATTN: see the haskell overlay for some overrides we need.

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
