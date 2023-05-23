{ depot, lib, pkgs, ... }:

let
  protoSrc = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "cloudapi";
    rev = "d45e30883ea8a2186ed3375305eddc9c9c504c13";
    sha256 = "01jcs3si4sllq685si8aby3g1lxjj46r1b7abmhak0m7xmgj0fb1";
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
