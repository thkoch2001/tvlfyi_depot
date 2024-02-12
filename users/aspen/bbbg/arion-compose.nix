{ ... }:

{
  services = {
    postgres.service = {
      image = "postgres:12";
      environment = {
        POSTGRES_DB = "bbbg";
        POSTGRES_USER = "bbbg";
        POSTGRES_PASSWORD = "password";
      };
      ports = [ "5432:5432" ];
    };
  };
}
