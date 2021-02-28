{ depot, pkgs, lib, ... }:

let

  inherit (depot.nix)
    buildC
    getBins
    ;

  inherit (pkgs)
    writeText
    runCommandNoCC
    glibc
    ;

  bins = getBins glibc [ "ldd" ];

  # Note especially that random is a symbol that is present
  # twice in the dependency tree: once in libc and once in
  # transitive. Our ordering ensures that the link flags are
  # resolved to `-ldep -ltrans -lc`. This ensures that
  # a) the missing symbol `random` in libdep.a is caught and
  #    the linker starts looking for a place to resolve it.
  # b) `random` is correctly resolved to libtrans.a over
  #    libc.a because of the ordering of the `-l` flags.
  #    We currently always prefer any dependency over the libc.
  # One caveat is that this doesn't seem to work for dynamic
  # linking since the dynamic linker doesn't care about the
  # order of the `-l` flags at compile time apparently.
  # This is however just an observation so far and needs
  # more investigation.
  transitive = buildC.library {
    name = "transitive-dependency";
    libName = "trans";

    include = [
      (writeText "trans.h" ''
        #pragma once
        int random(void);
      '')
    ];

    srcs = [
      (writeText "trans.c" ''
        #include <trans.h>
        int random(void) {
          return 42;
        }
      '')
    ];
  };

  dependency = buildC.library {
    name = "dependency-library";
    libName = "dep";

    deps = [
      transitive
    ];

    include = [
      (writeText "dep.h" ''
        #pragma once
        int double_random(void);
      '')
    ];

    srcs = [
      (writeText "dep.c" ''
        #include <trans.h>
        #include <dep.h>

        int double_random(void) {
          return (2 * random());
        }
      '')
    ];
  };

  prog = buildC.program {
    name = "static-test-program";

    deps = [
      dependency
    ];

    include = [];

    srcs = [
      (writeText "main.c" ''
        #include <stdio.h>
        #include <dep.h>

        int main(void) {
          printf("%d\n", double_random());
        }
      '')
    ];
  };

  checkIsStatic = extra: x:
    runCommandNoCC "buildC-test-static-${extra}" {} ''
      ( ${bins.ldd} "${x}/bin/${x.progName}" 2>&1 || exit 0 ) | \
        grep "not a dynamic executable"
      touch $out
    '';

  checkForLibs = extra: x:
    let
      # get deps and deps of deps which in our case
      # should include the overridden versions of
      # dependency and transitive.
      libs = x.deps
        ++ lib.concatMap (d: d.deps) x.deps;
      file = "${x}/bin/${x.progName}";
    in
      runCommandNoCC "buildC-test-dynamic-${extra}" {} (''
        ${bins.ldd} ${file} | grep "libc.so\\(.[0-9]\\)\\? => /nix/store"
      '' + lib.concatMapStrings (l: ''
        ${bins.ldd} ${file} | grep "lib${l.libName}.so => ${l}/lib/lib${l.libName}.so"
      '' + ''
        touch $out
      '') libs);

in depot.nix.utils.drvTargets {
  clangMuslStatic = checkIsStatic "clang-musl"
    (prog.overrideBuildConf {
      static = true;
      compiler = buildC.compilers.clang;
      libc = buildC.libcs.musl;
    });

  gccMuslStatic = checkIsStatic "gcc-musl"
    (prog.overrideBuildConf {
      static = true;
      compiler = buildC.compilers.gcc;
      libc = buildC.libcs.musl;
    });

  clangMuslDynamic = checkForLibs "clang-musl"
    (prog.overrideBuildConf {
      static = false;
      compiler = buildC.compilers.clang;
      libc = buildC.libcs.musl;
    });

  clangGlibcDynamic = checkForLibs "clang-glibc"
    (prog.overrideBuildConf {
      static = false;
      compiler = buildC.compilers.clang;
      libc = buildC.libcs.glibc;
    });

  gccMuslDynamic = checkForLibs "gcc-musl"
    (prog.overrideBuildConf {
      static = false;
      compiler = buildC.compilers.gcc;
      libc = buildC.libcs.musl;
    });

  gccGlibcDynamic = checkForLibs "gcc-glibc"
    (prog.overrideBuildConf {
      static = false;
      compiler = buildC.compilers.gcc;
      libc = buildC.libcs.glibc;
    });
}
