# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2021-04-09.
  nixpkgsCommit = "9e377a6ce42dccd9b624ae4ce8f978dc892ba0e2";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsCommit}.tar.gz";
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };
  nixpkgs = import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;

    # Lutris depends on p7zip, which is considered insecure.
    config.permittedInsecurePackages = [
      "p7zip-16.02"
    ];
  };

  # Tracking nixos-20.09 as of 2021-04-09.
  stableCommit = "d6f63659a7021051a46035373ed50fbea7e4e924";
  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stableCommit}.tar.gz";
    sha256 = "0vblhzg57sfzqpdm24lgs08vjv2204lzcp6hv4cbjd20rz0mxs4y";
  };
  stableNixpkgs = import stableNixpkgsSrc {};

  exposed = import ./nixpkgs-exposed/exposed { inherit nixpkgs stableNixpkgs; };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  inherit nixpkgsCommit nixpkgsSrc stableNixpkgsSrc;

  # Expose upstream attributes so they can be overridden in readTree nodes
  originals = {
    inherit (nixpkgs) gtest openldap go grpc notmuch rr;
    inherit (stableNixpkgs) git tdlib;
    ffmpeg = nixpkgs.ffmpeg-full;
    telega = stableNixpkgs.emacsPackages.telega;

  };

  # binary releases of dhall tools, since the build in nixpkgs is broken most of the time.
  # The binaries are also fully static builds, instead of the half-static crap that nixpkgs produces.
  easy-dhall-nix =
    import (builtins.fetchTarball {
      url = "https://github.com/justinwoo/easy-dhall-nix/archive/eae7f64c4d6c70681e5a56c84198236930ba425e.tar.gz";
      sha256 = "1y2x15v8a679vlpxazjpibfwajp6zph60f8wjcm4xflbvazk0dx7";
    }) { pkgs = nixpkgs; };

  dhall = self.easy-dhall-nix.dhall-simple;
  dhall-bash = self.easy-dhall-nix.dhall-bash-simple;
  dhall-docs = self.easy-dhall-nix.dhall-docs-simple;
  dhall-json = self.easy-dhall-nix.dhall-json-simple;
  dhall-lsp-server = self.easy-dhall-nix.dhall-lsp-simple;
  dhall-nix = self.easy-dhall-nix.dhall-nix-simple;
  # not yet in dhall-simple
  # dhall-nixpkgs = self.easy-dhall-nix.dhall-nixpkgs-simple;
  dhall-yaml = self.easy-dhall-nix.dhall-yaml-simple;

  # Use LLVM 11
  llvmPackages = nixpkgs.llvmPackages_11;
  clangStdenv = nixpkgs.llvmPackages_11.stdenv;
  stdenv = nixpkgs.llvmPackages_11.stdenv;

  clang-tools = (nixpkgs.clang-tools.override {
    llvmPackages = nixpkgs.llvmPackages_11;
  });

  # Provide Emacs 27
  #
  # The assert exists because the name of the attribute is unversioned
  # (which is different from previous versions).
  emacs27 = assert ((exposed.lib.versions.major nixpkgs.emacs.version) == "27");
    nixpkgs.emacs.overrideAttrs(old: {
      configureFlags = old.configureFlags ++ [ "--with-cairo" ];
    });

  emacs27-nox = assert ((exposed.lib.versions.major nixpkgs.emacs.version) == "27");
    nixpkgs.emacs-nox;

  # Make NixOS available
  nixos = import "${nixpkgsSrc}/nixos";
})
