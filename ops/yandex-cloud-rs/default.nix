{ depot, lib, pkgs, ... }:

let
  protoSrc = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "cloudapi";
    rev = "ca3d23bc2e9e56042a23a6cf0bade98b21aa0f31";
    sha256 = "1qsmycd0nkbskb9dj36l45914b0hpr6vpymn47vwa6qsfqraxj1m";
  };
in
pkgs.rustPlatform.buildRustPackage rec {
  name = "yandex-cloud-rs";
  src = depot.third_party.gitignoreSource ./.;
  cargoLock.lockFile = ./Cargo.lock;
  YANDEX_CLOUD_PROTOS = "${protoSrc}";
  nativeBuildInputs = [ pkgs.protobuf ];

  # The generated doc comments contain lots of things that rustc
  # *thinks* are doctests, but are actually just garbage leading to
  # compiler errors.
  doCheck = false;
}
