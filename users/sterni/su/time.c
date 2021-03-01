#define _POSIX_C_SOURCE 1 /* tzset() */
#define _XOPEN_SOURCE 1   /* timezone, daylight */
#include <time.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include <su/s.h>
#include <su/time.h>

// UTC offset

long su_local_utc_offset_secs_tm(struct tm local) {
  tzset();

  // timezone is *west* of UTC for some cursed reason
  long real_offset = -timezone;

  if(daylight && local.tm_isdst) {
    // TODO(sterni): verify that this is always true for POSIX
    real_offset += 3600;
  }

  return real_offset;
}

long su_local_utc_offset_secs(time_t t) {
  struct tm result;
  memset(&result, 0, sizeof(struct tm));

  if(localtime_r(&t, &result) != NULL) {
    return su_local_utc_offset_secs_tm(result);
  } else {
    // localtime_r should set errno
    return 0;
  }
}

long su_local_utc_offset_hours(time_t t) {
  long hours = lround(su_local_utc_offset_secs(t) / 3600);
  return hours;
}

// dot time -> struct tm

int su_dot_time_tm(su_dot_time_t dot, struct tm *tm) {
  memset(tm, 0, sizeof(struct tm));
  if(gmtime_r(&dot.dt_timestamp, tm) == NULL) {
    return -1;
  }

  return 0;
}

// dot time construction

su_dot_time_t su_local_dot_time(time_t t) {
  su_dot_time_t output;
  memset(&output, 0, sizeof(su_dot_time_t));

  output.dt_timestamp = t;
  output.dt_offset = su_local_utc_offset_hours(t);

  return output;
}

su_dot_time_t su_local_dot_time_now(void) {
  return su_local_dot_time(time(NULL));
}

// dot time rendering
int su_append_dot_time(s_t *s, struct tm t) {
  if(s_cap_inc(s, 2 + 2 + 2 + 1)
      || s_append_int_decimal(s, t.tm_hour, 2) == -1
      || s_append(s, slice_from_const("·")) == -1
      || s_append_int_decimal(s, t.tm_min, 2) == -1) {
    return -1;
  }

  return 0;
}

int su_append_dot_time_offset(s_t *s, int offset) {
  if(s_cap_inc(s, 4) == -1
      || s_append_char(s, offset >= 0 ? '+' : '-')
      || s_append_int_decimal(s, offset, 2)) {
    return -1;
  }

  return 0;
}

s_t su_render_dot_time(su_dot_time_t t) {
  struct tm tm;
  s_t output = s_new(2 + 2 + 2 + 3);

  if(su_dot_time_tm(t, &tm) != -1) {
    su_append_dot_time(&output, tm);
    su_append_dot_time_offset(&output, t.dt_offset);
  }

  return output;
}
