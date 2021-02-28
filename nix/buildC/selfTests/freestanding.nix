{ depot, pkgs, lib, ... }:

# Note: freestanding support is still very limited in buildC,
# so this is a snapshot of what is working already.

let
  inherit (depot.nix)
    utils
    buildC
    getBins
    ;

  inherit (pkgs)
    writeText
    runCommandNoCC
    llvmPackages
    ;

  bins = getBins llvmPackages.bintools-unwrapped [ "objdump" ];

  clib = buildC.library {
    name = "freestanding-lib";
    libName = "freestanding";

    buildConf = {
      libc = null;
    };

    include = [
      (writeText "freestanding.h" ''
        #pragma once
        int add(int, int);
      '')
    ];

    srcs = [
      (writeText "freestanding.c" ''
        int add(int a, int b) {
          return (a + b);
        }
      '')
    ];
  };

  checkForSymbols = drv:
    let
      want = [ "add" ];
      libfile = "${lib.getLib drv}/lib/lib${drv.libName}.a";
    in
    runCommandNoCC "symbols-check-${drv.name}" {}
      ((lib.concatMapStrings (sym: ''
        ${bins.objdump} -t "${libfile}" | grep '${sym}$'
      '') want) + ''
        ${bins.objdump} -t "${libfile}" > "$out"
      '');

in utils.drvTargets {
  libStaticClang = checkForSymbols
    (clib.overrideBuildConf {
      static = true;
      compiler = buildC.compilers.clang;
    });

  libStaticGcc = checkForSymbols
    (clib.overrideBuildConf {
      static = true;
      compiler = buildC.compilers.gcc;
    });
}
