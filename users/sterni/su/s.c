#include <su/char.h>
#include <su/s.h>

#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX(a, b) a > b ? a : b

int s_cap_set(s_t *s, s_int cap) {
  if(s->s_cap >= cap) {
    return 0;
  }

  char *tmp = realloc(s->s_buf, sizeof(char) * cap);

  if(tmp == NULL) {
    errno = ENOMEM;
    return -1;
  }

  s->s_buf = tmp;
  s->s_cap = cap;

  return 0;
}

int s_cap_inc(s_t *s, s_int cap) {
  return s_cap_set(s, s->s_cap + cap);
}

s_t s_new(s_int cap) {
  s_t s = {
    .s_len = 0,
    .s_buf = NULL,
    .s_cap = 0,
  };

  s_cap_set(&s, cap);

  return s;
}

void s_free(s_t *s) {
  s->s_cap = 0;
  s->s_len = 0;

  if(s->s_buf != NULL) {
    free(s->s_buf);
    s->s_buf = NULL;
  }
}

s_t s_from(const char *str, s_int len) {
  // we allocate one additional byte to make s_to0 cheaper
  s_t s = s_new(len + 1);

  if(s.s_cap >= len) {
    memcpy(s.s_buf, str, sizeof(char) * len);
    s.s_len = len;
  }

  return s;
}

s_t s_from0(const char *str) {
  s_int len = strlen(str);
  return s_from(str, len);
}

char *s_to0(s_t *s) {
  if(s_cap_inc(s, 1) == 0) {
    s->s_buf[s->s_len] = '\0';
    return s->s_buf;
  } else {
    return NULL;
  }
}

slice_t slice_from(const char *str, s_int len) {
  slice_t slice = {
    .slice_len = len,
    .slice_buf = str,
  };
  return slice;
}

int s_append(s_t *s, slice_t app) {
  s_int needed = s->s_len + app.slice_len;
  if(s->s_cap < needed) {
    // leave space for a potential NUL byte
    int res = s_cap_set(s, needed + 1);

    if(res != 0) {
      return res;
    }
  }

  memcpy(s->s_buf + s->s_len, app.slice_buf, app.slice_len);
  s->s_len = needed;

  return 0;
}

int s_append_char(s_t *s, char c) {
  return s_append(s, slice_from(&c, 1));
}

int s_append_int_decimal(s_t *s, int num, int digits) {
  int sign = 0;

  if(num < 0) {
    sign = 1;
    num = abs(num);
  }

  if(digits <= 0) {
    digits = MAX(floor(log(num) / log(10)), 1);
  }

  // digits + potential NUL byte
  if(s_cap_inc(s, (s_int) digits + sign + 1) == -1) {
    return -1;
  }

  if(sign) {
    s_append_char(s, '-');
  }

  for(int i = digits - 1; i >= 0; i--) {
    int base  = (int) lround(pow(10, i));
    int digit = (num / base) % 10;
    if(s_append_char(s, su_to_dec_digit((s_int) digit)) == -1) {
      return -1;
    }
  }

  return 0;
}

bool s_empty(s_t s) {
  return (s.s_len <= 0 || s.s_cap <= 0 || s.s_buf == NULL);
}

bool s_has_cap(s_t s, s_int cap) {
  return (s.s_buf != NULL && s.s_cap >= cap);
}

slice_t s_slice(s_t s) {
  return slice_from(s.s_buf, s.s_len);
}

bool slice_eq(slice_t a, slice_t b) {
  return (a.slice_len == b.slice_len
    && strncmp(a.slice_buf, b.slice_buf, a.slice_len) == 0);
}

void slice_write(slice_t s, FILE *target) {
  fwrite(s.slice_buf, s.slice_len, sizeof(char), target);
}

void slice_writeln(slice_t s, FILE *target) {
  slice_write(s, target);
  fputc('\n', target);
}

void slice_writefd(slice_t s, int fd) {
  (void) write(fd, s.slice_buf, s.slice_len);
  // TODO(sterni): handler written < slice_len
}
