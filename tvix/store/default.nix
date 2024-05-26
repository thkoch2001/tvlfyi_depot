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

(depot.tvix.crates.workspaceMembers.tvix-store.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
  '';
}).overrideAttrs {
  meta.ci = {
    targets = [ "integration-tests" "feature-permutations" ];
    extraSteps.import-docs = (mkImportCheck "tvix/store/docs" ./docs);
  };
  passthru = {
    integration-tests = depot.tvix.crates.workspaceMembers.tvix-store.build.override (old: {
      runTests = true;
      testPreRun = ''
        export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
        export PATH="$PATH:${pkgs.lib.makeBinPath [ pkgs.cbtemulator pkgs.google-cloud-bigtable-tool ]}"
      '';
      # enable some optional features.
      features = old.features ++ [ "cloud" "integration" ];
    });
    feature-permutations = pkgs.symlinkJoin {
      name = "tvix-store-feature-permutations";
      postBuild = "rm -rf $out/*"; # We want to clean up $out
      paths = (map
        (featuresPowerset: depot.tvix.crates.workspaceMembers.tvix-store.build.override ({
          runTests = true;
          testPreRun = ''
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          '';
          features = featuresPowerset;
        }))
        (depot.tvix.utils.powerset ([ "cloud" "fuse" "otlp" "tonic-reflection" ]
          # virtiofs feature currently fails to build on Darwin
          ++ lib.optional pkgs.stdenv.isLinux "virtiofs")
        ));
    };
  };
}
