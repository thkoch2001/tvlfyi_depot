#define _POSIX_C_SOURCE 200809L /* open_memstream */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <su/memstream.h>

int su_memstream_init(su_memstream_t *stream) {
  memset(stream, 0, sizeof(su_memstream_t));

  stream->m_hdl = NULL;
  stream->m_buf = NULL;
  stream->m_len = 0;

  stream->m_hdl = open_memstream(&(stream->m_buf), &(stream->m_len));

  if(stream->m_hdl == NULL) {
    return -1;
  } else {
    return 0;
  }
}

void su_memstream_flush(su_memstream_t *stream) {
  if(stream->m_hdl != NULL) {
    fflush(stream->m_hdl);
  }
}

void su_memstream_close_hdl(su_memstream_t *stream) {
  if(stream->m_hdl != NULL) {
    fclose(stream->m_hdl);
    stream->m_hdl = NULL;
  }
}

void su_memstream_free_buf(su_memstream_t *stream) {
  if(stream->m_buf != NULL) {
    free(stream->m_buf);
    stream->m_buf = NULL;
    stream->m_len = 0;
  }
}

void su_memstream_free(su_memstream_t *stream) {
  su_memstream_close_hdl(stream);
  su_memstream_free_buf(stream);
}

slice_t su_memstream_slice(su_memstream_t *stream) {
  su_memstream_flush(stream);
  slice_t slice = { stream->m_len, stream->m_buf };
  return slice;
}
