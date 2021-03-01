#ifndef SU_MEMSTREAM_H
#define SU_MEMSTREAM_H

#include <su/s.h>

typedef struct su_memstream {
  size_t  m_len;
  char   *m_buf;
  FILE   *m_hdl;
} su_memstream_t;

int su_memstream_init(su_memstream_t *);

void su_memstream_flush(su_memstream_t *);
void su_memstream_free(su_memstream_t *);
void su_memstream_close_hdl(su_memstream_t *);
void su_memstream_free_buf(su_memstream_t *);

slice_t su_memstream_slice(su_memstream_t *);

#endif
