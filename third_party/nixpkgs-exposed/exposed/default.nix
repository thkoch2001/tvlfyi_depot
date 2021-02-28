# This file has to be in yet another subdir
# because of how readTree interprets .skip-subtree
# see https://b.tvl.fyi/issues/89
{ nixpkgs, stableNixpkgs }:
{
  # Inherit the packages from nixos-unstable that should be available inside
  # of the repo. They become available under `pkgs.third_party.<name>`
  inherit (nixpkgs)
    age
    autoconf
    autoreconfHook
    avrdude
    avrlibc
    bashInteractive
    bat
    bc
    bind
    binutils-unwrapped
    buildBazelPackage
    buildFHSUserEnv
    buildGoModule
    buildGoPackage
    buildPackages
    buildRustCrate
    buildkite-agent
    busybox
    bzip2
    c-ares
    cacert
    cachix
    cairo
    cargo
    cgit
    clang_11
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
    fdtools
    fetchFromGitHub
    fetchgit
    fetchurl
    fetchzip
    findutils
    fira
    fira-code
    fira-mono
    flamegraph
    fontconfig
    freetype
    gcc-unwrapped
    gettext
    glibc
    gmock
    gnused
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
    jdk11
    jdk11_headless
    jetbrains-mono
    jq
    kontemplate
    lib
    libredirect
    linuxPackages
    linuxPackages_5_11
    luajit
    lutris
    makeFontsConf
    makeWrapper
    mandoc
    mdbook
    meson
    mime-types
    mkShell
    moreutils
    musl
    nano
    ncurses
    nginx
    ninja
    nix
    ocamlPackages
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
    quassel
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
    tree-sitter
    which
    writers
    writeShellScript
    writeShellScriptBin
    writeText
    xorg
    xz
    zlib
    zstd;

  # Inherit packages from the stable channel for things that are
  # broken on unstable
  inherit (stableNixpkgs)
    awscli # TODO(grfn): Move back to unstable once it is fixed
    ;

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
    ;

  haskellPackages = (nixpkgs.haskellPackages.override {
    overrides = (import ../haskell_overlay { pkgs = nixpkgs; });
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
}
