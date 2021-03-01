#ifndef SU_S_H
#define SU_S_H

#include <stdbool.h>
#include <stdio.h>

#define s_int size_t

typedef struct s {
  s_int  s_len;
  char  *s_buf;
  s_int  s_cap;
} s_t;

typedef struct slice {
  s_int       slice_len;
  const char *slice_buf;
} slice_t;

/* creating strings */

/// Allocate a new string and copy the given
/// string into it. Must either be a stack
/// allocated string or string literal of fixed
/// size that is zero-terminated.
/// Note: this doesn't work for pointers to
/// string literals since sizeof can't tell how
/// big they are.
// Note: the cast is necessary if we want to support
// char [] on the stack, but a bit scary.
#define s_from_const(s) \
  s_from((char *) s, sizeof(s) - 1)

s_t s_from(const char *, s_int);

s_t s_from0(const char *);

s_t s_new(s_int);

s_t s_copy(s_t);

/* altering strings */

int s_append(s_t *, slice_t);
int s_append_char(s_t *, char);
int s_append_int_decimal(s_t *, int, int);

// TODO(sterni) int s_replace(s_t *, slice_t);
// necessary? int s_replace_safe(s_t *, slice_t);

/* string memory management */

void s_free(s_t *);

int s_cap_set(s_t *, s_int);
int s_cap_inc(s_t *, s_int);

/* predicates */

bool s_empty(s_t);
bool s_has_cap(s_t, s_int);

/* conversion */

char *s_to0(s_t *);

slice_t s_slice(s_t);

/* slices */

/// Create a slice from a stack allocated string
/// or string literal of fixed size. Must be zero-
/// terminated.
/// Note: this doesn't work for pointers to
/// string literals since sizeof can't tell how
/// big they are.
#define slice_from_const(s) \
  slice_from((char *) s, sizeof(s) - 1)

slice_t slice_from(const char *, s_int);

slice_t slice_take(slice_t, s_int);

slice_t slice_drop(slice_t, s_int);

bool slice_eq(slice_t, slice_t);

void slice_write(slice_t, FILE *);
void slice_writeln(slice_t, FILE *);
void slice_writefd(slice_t, int);

#endif
