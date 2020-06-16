# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2020-06-10.
  nixpkgsCommit = "467ce5a9f45aaf96110b41eb863a56866e1c2c3c";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${nixpkgsCommit}.tar.gz";
    sha256 = "0qz7wgi61pdb335n18xm8rfwddckwv0vg8n7fii5abrrx47vnqcj";
  };
  nixpkgs = import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;
  };

  # Tracking nixos-20.03 as of 2020-05-22
  stableCommit = "48723f48ab92381f0afd50143f38e45cf3080405";
  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${stableCommit}.tar.gz";
    sha256 = "0h3b3l867j3ybdgimfn76lw7w6yjhszd5x02pq5827l659ihcf53";
  };
  stableNixpkgs = import stableNixpkgsSrc {};

  exposed = {
    # Inherit the packages from nixos-unstable that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`
    inherit (nixpkgs)
      age
      autoconf
      autoreconfHook
      awscli
      bashInteractive
      bat
      buildBazelPackage
      buildFHSUserEnv
      buildGoModule
      buildGoPackage
      buildPackages
      bzip2
      c-ares
      cacert
      cachix
      cairo
      cargo
      cgit
      clang-tools
      clang_10
      cmake
      coreutils
      cudatoolkit
      darwin
      dockerTools
      fetchFromGitHub
      fetchgit
      fetchurl
      fetchzip
      fira
      fira-code
      fira-mono
      fontconfig
      freetype
      gettext
      glibc
      gmock
      gnutar
      go
      google-cloud-sdk
      graphviz
      gzip
      haskell
      iana-etc
      imagemagickBig
      installShellFiles
      jdk
      jetbrains-mono
      jq
      kontemplate
      lib
      libredirect
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
      postgresql
      pounce
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
      stern
      symlinkJoin
      systemd
      tdlib
      terraform_0_12
      texlive
      thttpd
      tree
      which
      writeShellScript
      writeShellScriptBin
      writeText
      writeTextFile
      xorg
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

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  inherit nixpkgsCommit nixpkgsSrc stableNixpkgsSrc;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) grpc notmuch;
    inherit (stableNixpkgs) git;
    ffmpeg = nixpkgs.ffmpeg-full;
  };

  # Use LLVM 10
  llvmPackages = nixpkgs.llvmPackages_10;
  clangStdenv = nixpkgs.llvmPackages_10.stdenv;
  stdenv = nixpkgs.llvmPackages_10.stdenv;

  # The Go authors have released a version of Go (in alpha) that has a
  # type system. This makes it available, specifically for use with
  # //nix/buildTypedGo.
  typedGo = nixpkgs.go.overrideAttrs(old: {
    version = "dev-go2go";
    doCheck = false;
    patches = []; # they all don't apply and are mostly about Darwin crap

    nativeBuildInputs = [
      nixpkgs.git
    ];

    src = nixpkgs.fetchgit {
      url = "https://go.googlesource.com/go";
      leaveDotGit = true;
      # You might think these hashes are trivial to update. It's just
      # a branch in a git repository, right?
      #
      # Well, think again. Somehow I managed to get no fewer than 3
      # (!) different commit hashes for the same branch by cloning
      # this repository thrice. Only the third one (which you, the
      # reader, can find below for your reading pleasure) actually
      # gave me `go tool go2go`.
      rev = "ad307489d41133f32c779cfa1b0db4a852ace047";
      sha256 = "1rxr53cklnhmps7vrx1v43a16fr689h4v3ngdppzbdv4f23mq9pg";
    };
  });

  # Make NixOS available
  nixos = import "${nixpkgsSrc}/nixos";
})
