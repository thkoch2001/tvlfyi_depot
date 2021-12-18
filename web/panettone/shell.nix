{ depot ? import ../.. { } }:

with depot.third_party;

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
