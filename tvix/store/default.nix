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

(depot.tvix.crates.workspaceMembers.tvix-store.build.override (old: {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=/dev/null
  '';
  features = old.features
    # virtiofs feature currently fails to build on Darwin
    ++ lib.optional pkgs.stdenv.isLinux "virtiofs";
})).overrideAttrs (old: rec {
  meta.ci = {
    targets = [ "integration-tests" ] ++ lib.filter (x: lib.hasPrefix "with-features" x || x == "no-features") (lib.attrNames passthru);
    extraSteps.import-docs = (mkImportCheck "tvix/docs/src/store" ../docs/src/store);
  };
  passthru = (depot.tvix.utils.mkFeaturePowerset {
    inherit (old) crateName;
    features = ([ "cloud" "fuse" "otlp" "tonic-reflection" "xp-store-composition" ]
      # virtiofs feature currently fails to build on Darwin
      ++ lib.optional pkgs.stdenv.isLinux "virtiofs");
    override.testPreRun = ''
      export SSL_CERT_FILE=/dev/null
    '';
  }) // {
    integration-tests = depot.tvix.crates.workspaceMembers.${old.crateName}.build.override (old: {
      runTests = true;
      testPreRun = ''
        export SSL_CERT_FILE=/dev/null
        export PATH="$PATH:${pkgs.lib.makeBinPath [ pkgs.cbtemulator pkgs.google-cloud-bigtable-tool ]}"
      '';
      features = old.features ++ [ "integration" ];
    });
  };
})
