{
  depot,
  lib,
  pkgs,
  ...
}:

let
  protoSrc = pkgs.fetchFromGitHub {
    owner = "yandex-cloud";
    repo = "cloudapi";
    rev = "b4383be5ebe360bd946e49c8eaf647a73e9c44c0";
    sha256 = "0z4jyw2cylvyrq5ja8pcaqnlf6lf6ximj85hgjag6ckawayk1rzx";
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
