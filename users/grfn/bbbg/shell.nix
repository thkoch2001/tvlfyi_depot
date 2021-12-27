let
 depot = import ../../.. {};
in
with depot.third_party.nixpkgs;

mkShell {
  buildInputs = [
    arion
    depot.third_party.clj2nix
    clojure
    openjdk11_headless
    postgresql_12
    nix-prefetch-git
    (writeShellScriptBin "terraform" ''
      set -e
      module=$(nix-build ~/code/depot -A users.grfn.bbbg.tf.module)
      rm -f ~/tfstate/bbbg/*.json
      cp ''${module}/*.json ~/tfstate/bbbg
      exec ${depot.users.grfn.bbbg.tf.terraform}/bin/terraform \
        -chdir=/home/grfn/tfstate/bbbg \
        "$@"
    '')
  ];

  PGHOST = "localhost";
  PGUSER = "bbbg";
  PGDATABASE = "bbbg";
  PGPASSWORD = "password";
}
