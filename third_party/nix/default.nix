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

  src = ./.;

  # Proto generation in CMake is theoretically possible, but that is
  # very theoretical - this does it in Nix instead.
  protoSrcs = pkgs.runCommand "nix-proto-srcs" {} ''
    export PROTO_SRCS=${src}/src/proto
    mkdir -p $out/libproto
    ${pkgs.protobuf}/bin/protoc -I=$PROTO_SRCS \
      --cpp_out=$out/libproto \
      --plugin=protoc-gen-grpc=${pkgs.grpc}/bin/grpc_cpp_plugin --grpc_out=$out/libproto \
      $PROTO_SRCS/*.proto
  '';
in pkgs.llvmPackages.libcxxStdenv.mkDerivation {
  pname = "tvix";
  version = "2.3.4";
  inherit src;

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
    abseil_cpp
    aws-s3-cpp
    brotli
    bzip2
    c-ares
    curl
    editline
    flex
    glog
    grpc
    immer
    libseccomp
    libsodium
    openssl
    protobuf
    sqlite
    xz
  ];

  propagatedBuildInputs = with pkgs; [
    boost
    largeBoehm
  ];

  # Forward the location of the generated Protobuf / gRPC files so
  # that they can be included by CMake.
  NIX_PROTO_SRCS = protoSrcs;

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
