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
  clickhoseLocalFixedAWS = pkgs.runCommand "clickhouse-local-fixed"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
    mkdir -p $out/bin
    makeWrapper ${pkgs.clickhouse}/bin/clickhouse-local $out/bin/clickhouse-local \
      --append-flags "-C ${clickhouseConfigAWS}"
  '';
in

depot.nix.readTree.drvTargets {
  inherit clickhoseLocalFixedAWS;
  parse-bucket-logs = pkgs.runCommand "archeology-parse-bucket-logs"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
    mkdir -p $out/bin
    makeWrapper ${(pkgs.writers.writeRust "parse-bucket-logs-unwrapped" {} ./parse_bucket_logs.rs)} $out/bin/archeology-parse-bucket-logs \
      --prefix PATH : ${pkgs.lib.makeBinPath [ clickhoseLocalFixedAWS ]}
  '';

  shell = pkgs.mkShell {
    name = "archeology-shell";
    packages = with pkgs; [ clickhouse rust-analyzer rustc rustfmt ];
  };
}
