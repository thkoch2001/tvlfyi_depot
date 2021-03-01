#ifndef SU_URL_H
#define SU_URL_H

#include <stdbool.h>

#include <su/s.h>

s_t su_urlencode(slice_t, bool, bool);
s_t su_urldecode(slice_t);

#endif
