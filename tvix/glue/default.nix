{ depot, pkgs, ... }:

(depot.tvix.crates.workspaceMembers.tvix-glue.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
  # Make C++ Nix available, to compare eval results against.
  testInputs = [ pkgs.nix ];
})
