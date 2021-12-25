{ depot, lib, pkgs, ... }:

let
  sourceFilter = name: type:
    let
      baseName = builtins.baseNameOf (builtins.toString name);
    in
      (baseName == "Cargo.toml")
      || (type == "directory" && baseName == "src")
      || (lib.hasSuffix ".rs" baseName)
    ;
in

pkgs.buildRustCrate rec {
  pname = "store-ref-scanner";
  crateName = "store-ref-scanner";
  version = "0.1.0";
  edition = "2021";
  src = lib.cleanSourceWith { filter = sourceFilter; src = ./.; };

  passthru.tests = pkgs.buildRustCrate {
    pname = "store-ref-scanner-tests";
    inherit crateName src version edition;
    buildTests = true;
    postInstall = ''
      set -ex
      export RUST_BACKTRACE=1
      # recreate a file hierarchy as when running tests with cargo
      # the source for test data
      # build outputs
      testRoot=target/debug
      mkdir -p $testRoot
      chmod +w -R .
      # test harness executables are suffixed with a hash,
      # like cargo does this allows to prevent name collision
      # with the main executables of the crate
      hash=$(basename $out)
      ls -lasR $out
      for file in $out/tests/*; do
        f=$testRoot/$(basename $file)-$hash
        cp $file $f
        $f 2>&1 | tee -a $out/tests.log
      done
      rm -rf $out/tests
    '';
  }

}
