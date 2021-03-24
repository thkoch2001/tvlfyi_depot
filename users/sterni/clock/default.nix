{ depot, ... }:

let

  inherit (depot.nix)
    buildC
    ;

  inherit (depot.third_party)
    writeText
    ;

  main = writeText "main.c" ''
    #include <stdio.h>
    #include <su/s.h>
    #include <su/time.h>

    int main(void) {
      su_dot_time_t now = su_local_dot_time_now();
      s_t now_rndr = su_render_dot_time(now);
      slice_write(s_slice(now_rndr), stdout);
      s_free(&now_rndr);
      return 0;
    }
  '';

in
  buildC.program {
    name = "clock";
    deps = [
      depot.users.sterni.su
    ];
    srcs = [
      main
    ];
    CFLAGS = [
      "-Wall"
      "-Wextra"
      "-Werror"
    ];
    meta.description = "dot time clock";
  }
