{ depot, pkgs, ... }:

pkgs.rustPlatform.buildRustPackage {
  name = "niri-reap";
  src = depot.third_party.gitignoreSource ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "niri-ipc-0.1.9" = "sha256:1s294bw62mmckq9xyfzgw4p2nvkzday4k276j60m668prhlfp071";
    };
  };
}
