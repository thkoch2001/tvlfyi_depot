{ depot, pkgs, lib, ... }:

lib.fix
  (self: depot.third_party.naersk.buildPackage (lib.fix (naerskArgs: {
    src = depot.third_party.gitignoreSource ./.;
    # see https://github.com/nix-community/naersk/issues/169
    root = depot.nix.sparseTree ./. [ ./Cargo.lock ./Cargo.toml ];

    # naersk doesn't resolve this, so we need to patch the Cargo.toml file
    prePatch = ''
      substituteInPlace Cargo.toml \
        --replace "../store" "${../store}"
    '';

    # make sure the protobuf compiler is available, and we actually
    # set PROTO_ROOT like we do when building the store crate.
    nativeBuildInputs = [ pkgs.protobuf ];
    PROTO_ROOT = depot.tvix.store.protoRoot;

    doCheck = true;
  }))
  )
