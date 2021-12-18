let
  depot = import ../../.. { };
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
  ];

  PGHOST = "localhost";
  PGUSER = "bbbg";
  PGDATABASE = "bbbg";
  PGPASSWORD = "password";
}
