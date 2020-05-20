{ pkgs ? (import <nixpkgs> {}).third_party, ... }:

let
  stdenv = with pkgs; overrideCC clangStdenv clang_9;

  aws-s3-cpp = pkgs.aws-sdk-cpp.override {
    apis = ["s3" "transfer"];
    customMemoryManagement = false;
  };

 # TODO(tazjin): this is copied from the original derivation, but what
 # is it for?
  largeBoehm = pkgs.boehmgc.override {
    enableLargeConfig = true;
  };
in stdenv.mkDerivation {
  pname = "nix";
  version = "2.3.4";
  src = ./.;

  nativeBuildInputs = with pkgs; [
    bison
    clang-tools
    cmake
    meson
    ninja
    pkgconfig
    libxml2
    libxslt
  ];

 # TODO(tazjin): Some of these might only be required for native inputs
  buildInputs = with pkgs; [
    aws-s3-cpp
    boost
    brotli
    bzip2
    curl
    editline
    flex
    glog
    largeBoehm
    libseccomp
    libsodium
    openssl
    sqlite
    xz
  ];

  mesonFlags = [
    "-Dsandbox_shell=${pkgs.busybox-sandbox-shell}/bin/busybox"
  ];

  # Install the various symlinks to the Nix binary which users expect
  # to exist.
  postInstall = ''
    ln -s $out/bin/nix $out/bin/nix-build
    ln -s $out/bin/nix $out/bin/nix-channel
    ln -s $out/bin/nix $out/bin/nix-collect-garbage
    ln -s $out/bin/nix $out/bin/nix-copy-closure
    ln -s $out/bin/nix $out/bin/nix-daemon
    ln -s $out/bin/nix $out/bin/nix-env
    ln -s $out/bin/nix $out/bin/nix-hash
    ln -s $out/bin/nix $out/bin/nix-instantiate
    ln -s $out/bin/nix $out/bin/nix-prefetch-url
    ln -s $out/bin/nix $out/bin/nix-shell
    ln -s $out/bin/nix $out/bin/nix-store

    mkdir -p $out/libexec/nix
    ln -s $out/bin/nix $out/libexec/nix/build-remote
  '';

  # TODO(tazjin): equivalent of --enable-gc
  # TODO(tazjin): integration test setup?
  # TODO(tazjin): docs generation?
}
