{ depot, lib, pkgs, ... }:

let
  parent = depot.users.zseri.store-ref-scanner;
in
pkgs.buildRustCrate {
  pname = "store-ref-scanner-tests";
  inherit (parent) crateName src version edition;
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
    set +ex
  '';
}
