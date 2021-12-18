{ pkgs, ... }:

{
  services = {
    postgres.service = {
      image = "postgres:11";
      environment = {
        POSTGRES_DB = "panettone";
        POSTGRES_USER = "panettone";
        POSTGRES_PASSWORD = "password";
      };
      ports = [ "5432:5432" ];
    };

    ldap.service = {
      image = "osixia/openldap:1.5.0";
      ports = [ "3899:389" ];
    };

    panettone-integration-tests = {
      image.contents = [
        pkgs.coreutils
        pkgs.postgresql
        (pkgs.writeShellScriptBin "run_integration_tests.sh" ''
          set -euo pipefail
          # Wait for postgres to be ready
          while ! < /dev/tcp/postgres/5432; do sleep 1; done
          echo "SELECT 'CREATE DATABASE panettone_test' WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = 'panettone_test')\gexec" | psql
          exec ${(import ../.. {}).web.panettone.integrationTests}
        '')
      ];

      service.environment = {
        RUN_INTEGRATION_TESTS = "1";
        PGHOST = "postgres";
        PGUSER = "panettone";
        PGPASSWORD = "password";
        LDAP_HOST = "ldap";
        LDAP_PORT = "389";
      };

      service.command = "run_integration_tests.sh";

      service.depends_on = [
        "postgres"
        "ldap"
      ];
    };
  };
}
