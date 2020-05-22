# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2020-05-21.
  commit = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
    sha256 = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
  };
  nixpkgs = import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;
  };

  # Tracking nixos-20.03 as of 2020-05-22
  stableCommit = "48723f48ab92381f0afd50143f38e45cf3080405";
  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
    sha256 = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
  };
  stableNixpkgs = import stableNixpkgsSrc {};

  exposed = {
    # Inherit the packages from nixos-usntable that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`
    inherit (nixpkgs)
      age
      autoconf
      bashInteractive
      bat
      buildGoModule
      buildGoPackage
      bzip2
      c-ares
      cacert
      cachix
      cairo
      cargo
      cgit
      clang-tools
      clangStdenv
      clang_10
      cmake
      coreutils
      cudatoolkit
      darwin
      dockerTools
      fetchFromGitHub
      fetchurl
      fetchzip
      fira
      fira-code
      fira-mono
      fontconfig
      freetype
      gettext
      glibc
      gnutar
      go
      google-cloud-sdk
      graphviz
      grpc
      gzip
      haskell
      iana-etc
      imagemagickBig
      jetbrains-mono
      jq
      kontemplate
      lib
      libredirect
      llvmPackages
      luajit
      luatex
      makeFontsConf
      makeWrapper
      mdbook
      meson
      mime-types
      moreutils
      nano
      nginx
      ninja
      nix
      openssh
      openssl
      overrideCC
      pandoc
      parallel
      pkgconfig
      pounce
      protobuf
      python3
      python3Packages
      remarshal
      rink
      ripgrep
      rsync
      runCommand
      runCommandNoCC
      rustPlatform
      rustc
      sbcl
      sqlite
      stdenv
      stern
      symlinkJoin
      systemd
      tdlib
      terraform_0_12
      texlive
      thttpd
      tree
      writeShellScript
      writeShellScriptBin
      writeText
      writeTextFile
      xz
      zlib
      zstd;

    # Inherit packages that should come from a stable channel
    inherit (stableNixpkgs)
      emacs26
      emacs26-nox
      emacsPackages
      emacsPackagesGen;

    # Required by //third_party/nix
    inherit (nixpkgs)
      aws-sdk-cpp
      bison
      boehmgc
      boost # urgh
      brotli
      busybox-sandbox-shell
      curl
      docbook5
      docbook_xsl_ns
      editline
      flex
      libseccomp
      libsodium
      libxml2
      libxslt
      mercurial
      perl
      perlPackages
      utillinuxMinimal;
  };

in exposed // {
  callPackage = nixpkgs.lib.callPackageWith exposed;

  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  inherit nixpkgsSrc stableNixpkgsSrc;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) abseil-cpp git glog notmuch;
    ffmpeg = nixpkgs.ffmpeg-full;
  };

  # Make NixOS available
  nixos = import "${nixpkgsSrc}/nixos";
}
