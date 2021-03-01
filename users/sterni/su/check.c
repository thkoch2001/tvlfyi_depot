#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <su/check.h>

void su_check_init(su_check_t *chk) {
  memset(chk, 0, sizeof(su_check_t));

  chk->check_result = true;
  chk->check_exit_on_fail = false;
  chk->check_name_width = 50;
  chk->check_output = stdout;
}

void su_check_finish(su_check_t *chk) {
  if(chk->check_result) {
    exit(EXIT_SUCCESS);
  } else {
    exit(EXIT_FAILURE);
  }
}

void print_right_pad(FILE *out, char *s, int width) {
  while(*s != '\0') {
    fputc(*s, out);
    width--; s++;
  }

  while(width > 0) {
    fputc(' ', out);
    width--;
  }
}

void su_check_assert(su_check_t *chk, char *name, bool res) {
  char *res_str = res ? " okay" : " FAIL";

  print_right_pad(chk->check_output, name, chk->check_name_width - 1);
  fputs(res_str, chk->check_output);
  fputc('\n', chk->check_output);

  chk->check_result = chk->check_result && res;

  if(!chk->check_result && chk->check_exit_on_fail) {
    exit(EXIT_FAILURE);
  }
}
