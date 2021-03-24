# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2021-03-25.
  nixpkgsCommit = "60dd94fb7e01a8288f6638eee71d7cb354c49327";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsCommit}.tar.gz";
    sha256 = "1j62nmzz3w33dplzf1xz1pg1pfkxii7lwdqmsxmc71cs9cm3s7n1";
  };
  nixpkgs = import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;

    # Lutris depends on p7zip, which is considered insecure.
    config.permittedInsecurePackages = [
      "p7zip-16.02"
    ];
  };

  # Tracking nixos-20.09 as of 2021-03-25.
  stableCommit = "223d0d733a66b46504ea6b4c15f88b7cc4db58fb";
  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stableCommit}.tar.gz";
    sha256 = "09wy33zbzxj33296ddrrb79630kxpj1c3kiv38zs4wrw24206c2v";
  };
  stableNixpkgs = import stableNixpkgsSrc {};

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
