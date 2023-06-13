{ depot, lib, pkgs, ... }:

let
  protoSrc = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "cloudapi";
    rev = "dcff396bf4bcdd724207f3b38add3a968d5e9125";
    sha256 = "0yi4wwqff33bja3nn90izn5r0ar97igpamkbk2kadsavpwp3dry2";
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
