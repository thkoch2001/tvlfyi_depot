#ifndef SU_URL_H
#define SU_URL_H

#include <stdbool.h>

#include <su/s.h>

s_t urlencode(slice_t, bool, bool);
s_t urldecode(slice_t);

#endif
