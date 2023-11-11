{ depot, pkgs, ... }:

depot.nix.readTree.drvTargets {
  parse-bucket-logs = pkgs.runCommand "archeology-parse-bucket-logs"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
    mkdir -p $out/bin
    makeWrapper ${(pkgs.writers.writeRust "parse-bucket-logs-unwrapped" {} ./parse_bucket_logs.rs)} $out/bin/archeology-parse-bucket-logs \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.clickhouse ]}
  '';

  shell = pkgs.mkShell {
    name = "archeology-shell";
    packages = with pkgs; [ clickhouse rust-analyzer rustc rustfmt ];
  };
}
