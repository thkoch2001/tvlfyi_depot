# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }@args:

let
  # Tracking nixos-unstable as of 2021-02-18.
  nixpkgsCommit = "6b1057b452c55bb3b463f0d7055bc4ec3fd1f381";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsCommit}.tar.gz";
    sha256 = "10qfg11g8m0q2k3ibcm0ivjq494gqynshm3smjl1rfn5ifjf5fz8";
  };
  nixpkgs = args.nixpkgs or (import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;

    # Lutris depends on p7zip, which is considered insecure.
    config.permittedInsecurePackages = [
      "p7zip-16.02"
    ];
  });

  # Tracking nixos-20.09 as of 2021-02-17.
  stableCommit = "5c53c720ff690ef82a9fe4849e7b70c104e1c82f";
  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stableCommit}.tar.gz";
    sha256 = "0gjxfxbfc6maqg48k9ai476s6zkc94p0y3v9yjgwbiy7b38pqfys";
  };
  stableNixpkgs = args.stableNixpkgs or (import stableNixpkgsSrc {});

  exposed = import ./nixpkgs-exposed/exposed { inherit nixpkgs stableNixpkgs; };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  inherit nixpkgsCommit nixpkgsSrc stableNixpkgsSrc;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) gtest openldap go grpc notmuch rr;
    inherit (stableNixpkgs) git tdlib;
    ffmpeg = nixpkgs.ffmpeg-full;
    telega = stableNixpkgs.emacsPackages.telega;
  };

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
