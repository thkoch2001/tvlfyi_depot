args@{ depot ? import ../../../.. { }
, pkgs ? depot.third_party.nixpkgs
, ...
}:

depot.third_party.naersk.buildPackage {
  name = "xanthous-server";
  version = "0.0.1";

  # TODO(grfn): restricted-eval breaks gitignoreSource here somehow, how?
  src = ./.;

  passthru = {
    docker = import ./docker.nix args;
  };
}
