#ifndef SU_TIME_H
#define SU_TIME_H

#include <time.h>

// dot time

typedef struct su_dot_time {
  /// UTC timestamp
  time_t dt_timestamp;
  /// UTC offset in hours (positive == east)
  long   dt_offset;
} su_dot_time_t;

su_dot_time_t su_local_dot_time(time_t);
su_dot_time_t su_local_dot_time_now(void);

s_t su_render_dot_time(su_dot_time_t);
s_t su_render_dot_datetime(su_dot_time_t);

s_t su_render_rfc3339_utc(su_dot_time_t);

// internal / libc glue API

long su_local_utc_offset_secs(time_t t);
long su_local_utc_offset_hours(time_t t);

#endif
