# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2020-11-21.
  nixpkgsCommit = "a322b32e9d74fb476944ff6cfb55833dc69cfaaa";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsCommit}.tar.gz";
    sha256 = "1r0mkiqxija75spnyksmh8x5j4smnrxv5f7768s81gsl570kls0l";
  };
  nixpkgs = import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;

    # Lutris depends on p7zip, which is considered insecure.
    config.permittedInsecurePackages = [
      "p7zip-16.02"
    ];
  };

  # Tracking nixos-20.09 as of 2020-11-21.
  stableCommit = "58f9c4c7d3a42c912362ca68577162e38ea8edfb";
  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stableCommit}.tar.gz";
    sha256 = "1517dy07jf4zhzknqbgm617lgjxsn7a6k1vgq61c67f6h55qs5ij";
  };
  stableNixpkgs = import stableNixpkgsSrc {};

  exposed = {
    # Inherit the packages from nixos-unstable that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`
    inherit (nixpkgs)
      age
      autoconf
      autoreconfHook
      avrdude
      avrlibc
      awscli
      bashInteractive
      bat
      buildBazelPackage
      buildFHSUserEnv
      buildGoModule
      buildGoPackage
      buildPackages
      buildkite-agent
      busybox
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
      dfu-programmer
      dfu-util
      diffutils
      docker-compose
      dockerTools
      emacs26
      emacs26-nox
      emacsPackages
      emacsPackagesGen
      execline
      fd
      fetchFromGitHub
      fetchgit
      fetchurl
      fetchzip
      fira
      fira-code
      fira-mono
      flamegraph
      fontconfig
      freetype
      gettext
      glibc
      gmock
      gnutar
      google-cloud-sdk
      graphviz
      gzip
      haskell
      iana-etc
      imagemagickBig
      installShellFiles
      jdk
      jdk11
      jetbrains-mono
      jq
      kontemplate
      lib
      libredirect
      linuxPackages
      luajit
      lutris
      makeFontsConf
      makeWrapper
      mdbook
      meson
      mime-types
      mkShell
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
      pkgsCross
      postgresql
      pounce
      pulseaudio
      python3
      python3Packages
      remarshal
      rink
      ripgrep
      rsync
      runCommand
      runCommandLocal
      runCommandNoCC
      rustPlatform
      rustc
      s6-portable-utils
      sbcl
      shellcheck
      sqlite
      stdenvNoCC
      stern
      symlinkJoin
      systemd
      tdlib
      teensy-loader-cli
      terraform_0_12
      texlive
      thttpd
      tree
      unzip
      which
      writeShellScript
      writeShellScriptBin
      writeText
      xorg
      xz
      zlib
      zstd;

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
      quassel
      utillinuxMinimal;

    haskellPackages = (nixpkgs.haskellPackages.override {
      overrides = (import ./haskell_overlay { pkgs = nixpkgs; });
    });

    gradle_6 = (nixpkgs.gradleGen.override {
      java = nixpkgs.jdk11;
      jdk = nixpkgs.jdk11;
    }).gradleGen rec {
      name = "gradle-6.5.1";
      nativeVersion = "0.22-milestone-3";

      src = builtins.fetchurl {
        url = "https://services.gradle.org/distributions/${name}-bin.zip";
        sha256 = "0jmmipjh4fbsn92zpifa5cqg5ws2a4ha0s4jzqhrg4zs542x79sh";
      };
    };
  };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  inherit nixpkgsCommit nixpkgsSrc stableNixpkgsSrc;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) gtest openldap go grpc notmuch rr;
    inherit (stableNixpkgs) git;
    ffmpeg = nixpkgs.ffmpeg-full;
  };

  # Use LLVM 10
  llvmPackages = nixpkgs.llvmPackages_10;
  clangStdenv = nixpkgs.llvmPackages_10.stdenv;
  stdenv = nixpkgs.llvmPackages_10.stdenv;

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

  # The Go authors have released a version of Go (in alpha) that has a
  # type system. This makes it available, specifically for use with
  # //nix/buildTypedGo.
  go = nixpkgs.go.overrideAttrs(old: {
    version = "dev-go2go";
    doCheck = false;
    patches = []; # they all don't apply and are mostly about Darwin crap

    src = nixpkgs.fetchgit {
      url = "https://go.googlesource.com/go";
      # You might think these hashes are trivial to update. It's just
      # a branch in a git repository, right?
      #
      # Well, think again. Somehow I managed to get no fewer than 3
      # (!) different commit hashes for the same branch by cloning
      # this repository thrice. Only the third one (which you, the
      # reader, can find below for your reading pleasure) actually
      # gave me `go tool go2go`.
      rev = "ad307489d41133f32c779cfa1b0db4a852ace047";
      leaveDotGit = true;
      sha256 = "1nxmqdlyfx7w3g5vhjfq24yrc9hwpsa2mjv58xrmhh8vvy50ziqq";

      postFetch = ''
        cd $out
        ${nixpkgs.git}/bin/git log -n 1 "--format=format:devel +%H %cd" HEAD > VERSION
        rm -rf .git
      '';
    };
  });

  # Make NixOS available
  nixos = import "${nixpkgsSrc}/nixos";
})
