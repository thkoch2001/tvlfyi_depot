{
  depot ? import ../.. { },
}:

with depot.third_party.nixpkgs;

mkShell {
  buildInputs = [
    docker-compose
    postgresql
  ];

  PGPASSWORD = "password";
  PGHOST = "localhost";
  PGUSER = "panettone";
  PGDATABASE = "panettone";
}
