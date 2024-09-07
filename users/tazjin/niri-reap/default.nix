{ depot, pkgs, ... }:

pkgs.rustPlatform.buildRustPackage {
  name = "niri-reap";
  src = depot.third_party.gitignoreSource ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "niri-ipc-0.1.8" = "sha256:0wyl0mpk9hg67bvj7q120wanrdqn3ls9zv9vjv9yxp11kan5pi1q";
    };
  };
}
