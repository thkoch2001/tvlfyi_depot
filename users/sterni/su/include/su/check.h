#ifndef SU_CHECK_H
#define SU_CHECK_H

#include <stdbool.h>

typedef struct check {
  bool  check_result;
  bool  check_exit_on_fail;
  int   check_name_width;
  FILE *check_output;
} check_t;

void check_init(check_t *);

void check_assert(check_t *, char *, bool);

void check_finish(check_t *);

#endif
