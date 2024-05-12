{ depot, pkgs, ... }:

let
  clickhouseConfigAWS = builtins.toFile "clickhouse-local.xml" ''
    <clickhouse>
        <s3>
          <use_environment_credentials>true</use_environment_credentials>
        </s3>
    </clickhouse>
  '';
  # clickhouse has a very odd AWS config concept.
  # Configure it to be a bit more sane.
  clickhouseLocalFixedAWS = pkgs.runCommand "clickhouse-local-fixed"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
    mkdir -p $out/bin
    makeWrapper ${pkgs.clickhouse}/bin/clickhouse-local $out/bin/clickhouse-local \
      --append-flags "-C ${clickhouseConfigAWS}"
  '';
in

depot.nix.readTree.drvTargets {
  inherit clickhouseLocalFixedAWS;
  parse-bucket-logs = pkgs.runCommand "archeology-parse-bucket-logs"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
    mkdir -p $out/bin
    makeWrapper ${(pkgs.writers.writeRust "parse-bucket-logs-unwrapped" {} ./parse_bucket_logs.rs)} $out/bin/archeology-parse-bucket-logs \
      --prefix PATH : ${pkgs.lib.makeBinPath [ clickhouseLocalFixedAWS ]}
  '';

  shell = pkgs.mkShell {
    name = "archeology-shell";
    packages = with pkgs; [ awscli2 clickhouseLocalFixedAWS rust-analyzer rustc rustfmt ];

    AWS_PROFILE = "sso";
    AWS_CONFIG_FILE = pkgs.writeText "aws-config" ''
      [sso-session nixos]
      sso_region = eu-north-1
      sso_start_url = https://nixos.awsapps.com/start
      sso_registration_scopes = sso:account:access

      [profile "sso"]
      sso_session = nixos
      sso_account_id = 080433136561
      sso_role_name = archeologist
    '';
  };
}
