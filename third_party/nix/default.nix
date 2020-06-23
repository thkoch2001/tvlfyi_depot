{ pkgs ? (import ../.. {}).third_party
, buildType ? "release", ... }:

let
  aws-s3-cpp = pkgs.aws-sdk-cpp.override {
    apis = ["s3" "transfer"];
    customMemoryManagement = false;
  };

 # TODO(tazjin): this is copied from the original derivation, but what
 # is it for?
  largeBoehm = pkgs.boehmgc.override {
    enableLargeConfig = true;
  };
in pkgs.llvmPackages.libcxxStdenv.mkDerivation {
  pname = "tazjix";
  version = "2.3.4";
  src = ./.;

  # Abseil's sources need to be symlinked into Nix' sources.
  postUnpack = ''
    ln -fs ${pkgs.abseil_cpp.drvAttrs.src} nix/abseil_cpp
    ln -fs ${pkgs.glog.drvAttrs.src} nix/glog
  '';

  nativeBuildInputs = with pkgs; [
    bison
    clang-tools
    cmake
    pkgconfig
    libxml2
    libxslt
    (import ./clangd.nix pkgs)
  ];

 # TODO(tazjin): Some of these might only be required for native inputs
  buildInputs = with pkgs; [
    aws-s3-cpp
    brotli
    bzip2
    curl
    editline
    flex
    libseccomp
    libsodium
    openssl
    sqlite
    xz
  ];

  installCheckInputs = with pkgs; [
    utillinux
  ];

  propagatedBuildInputs = with pkgs; [
    boost
    largeBoehm
  ];

  doInstallCheck = true;

  installCheckPhase = ''
    export PATH=$out/bin:$PATH
    cd ../tests/
    substituteAll common.sh.in common.sh
    bash lang.sh
  '';

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

  # TODO(tazjin): integration test setup?
  # TODO(tazjin): docs generation?
}
