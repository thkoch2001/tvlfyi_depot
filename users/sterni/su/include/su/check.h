#ifndef SU_CHECK_H
#define SU_CHECK_H

#include <stdbool.h>

typedef struct su_check {
  bool  check_result;
  bool  check_exit_on_fail;
  int   check_name_width;
  FILE *check_output;
} su_check_t;

void su_check_init(su_check_t *);

void su_check_assert(su_check_t *, char *, bool);

void su_check_finish(su_check_t *);

#endif
