{ depot ? import ../.. {} }:

with depot.third_party.nixpkgs;

mkShell {
  buildInputs = [
    arion
    postgresql
  ];

  PGPASSWORD = "password";
  PGHOST = "localhost";
  PGUSER = "panettone";
  PGDATABASE = "panettone";
}
