{ depot, pkgs, lib, ... }:

let
  mkImportCheck = p: expectedPath: {
    label = ":nix :import ${p} with tvix-store import";
    needsOutput = true;
    command = pkgs.writeShellScript "tvix-import-check" ''
      export BLOB_SERVICE_ADDR=memory://
      export DIRECTORY_SERVICE_ADDR=memory://
      export PATH_INFO_SERVICE_ADDR=memory://
      TVIX_STORE_OUTPUT=$(result/bin/tvix-store import ${p})
      EXPECTED='${/* the vebatim expected Tvix output: */expectedPath}'

      echo "tvix-store output: ''${TVIX_STORE_OUTPUT}"
      if [ "$TVIX_STORE_OUTPUT" != "$EXPECTED" ]; then
        echo "Correct would have been ''${EXPECTED}"
        exit 1
      fi

      echo "Output was correct."
    '';
  };
in

((depot.tvix.crates.workspaceMembers.tvix-store.build.override (old: {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
  '';
  features = old.features
    # virtiofs feature currently fails to build on Darwin
    ++ lib.optional pkgs.stdenv.isLinux "virtiofs";
})).overrideAttrs
  (oldAttrs: {
    meta.ci = {
      targets = [ "integration-tests" ];
      extraSteps.import-docs = mkImportCheck "tvix/docs/src/store" ../docs/src/store;
    };
    passthru.integration-tests = depot.tvix.crates.workspaceMembers.${oldAttrs.crateName}.build.override (old: {
      runTests = true;
      testPreRun = ''
        export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
        export PATH="$PATH:${lib.makeBinPath [ pkgs.cbtemulator pkgs.google-cloud-bigtable-tool ]}"
      '';
      features = old.features ++ [ "integration" ];
    });
  })).overrideAttrs
  (oldAttrs:
  (depot.tvix.utils.mkFeaturePowerset {
    inherit oldAttrs;
    features = [ "cloud" "fuse" "otlp" "tonic-reflection" ]
      # virtiofs feature currently fails to build on Darwin
      ++ lib.optional pkgs.stdenv.isLinux "virtiofs";
    override.testPreRun = ''
      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    '';
  }))
