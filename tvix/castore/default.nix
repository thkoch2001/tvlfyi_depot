{ depot, pkgs, ... }:

depot.tvix.crates.workspaceMembers.tvix-castore.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
    export PATH="$PATH:${pkgs.lib.makeBinPath [pkgs.cbtemulator pkgs.google-cloud-bigtable-tool]}"
  '';

  # enable some optional features.
  features = [ "default" "cloud" ];
}
