{ depot, ... }:

let

  inherit (depot.nix)
    buildC
    ;

  inherit (depot.third_party)
    writeText
    ;

  # Note especially that random is a symbol that is present
  # twice in the dependency tree: once in libc and once in
  # transitive. Our ordering ensures that the link flags are
  # resolved to `-ldep -ltrans -lc`. This ensures that
  # a) the missing symbol `random` in libdep.a is caught and
  #    the linker starts looking for a place to resolve it.
  # b) 
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
    buildConf = {
      static = true;
    };

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

in
  prog
