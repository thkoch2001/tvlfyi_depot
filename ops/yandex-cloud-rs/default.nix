{ depot, lib, pkgs, ... }:

let
  protoSrc = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "cloudapi";
    rev = "67589e7503bd8997b7aa27af86a81fb6b9eec655";
    sha256 = "0d8w98h58xzzcjq9s3y949gv5b4z4r05vgpz3n22asyfrfx6x6kf";
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
