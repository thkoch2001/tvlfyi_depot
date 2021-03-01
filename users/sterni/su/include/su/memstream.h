#ifndef SU_MEMSTREAM_H
#define SU_MEMSTREAM_H

#include <su/s.h>

typedef struct memstream {
  size_t  m_len;
  char   *m_buf;
  FILE   *m_hdl;
} memstream_t;

int memstream_init(memstream_t *);

void memstream_flush(memstream_t *);
void memstream_free(memstream_t *);
void memstream_close_hdl(memstream_t *);
void memstream_free_buf(memstream_t *);

slice_t memstream_slice(memstream_t *);

#endif
