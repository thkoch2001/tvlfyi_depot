#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <su/s.h>
#include <su/time.h>

int main(int argc, char **argv) {
  int newline = 1;
  int date = 0;

  for(int i = 1; i < argc; i++) {
    if(strcmp(argv[i], "-d") == 0) {
      date = 1;
    } else if(strcmp(argv[i], "-n") == 0) {
      newline = 0;
    } else {
      slice_write(
        slice_from_const(
          "Usage: clock [-n] [-d]\n\n"
          "  -d    display date and time\n"
          "  -n    don't print a newline after the time\n"
        ),
        stderr
      );
      exit(100);
    }
  }

  su_dot_time_t now = su_local_dot_time_now();
  s_t now_rndr = date ? su_render_dot_datetime(now) : su_render_dot_time(now);

  slice_write(s_slice(now_rndr), stdout);

  s_free(&now_rndr);

  if(newline) {
    fputc('\n', stdout);
  }

  return 0;
}
