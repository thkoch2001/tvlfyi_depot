#define _POSIX_C_SOURCE 200809L /* open_memstream */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <su/memstream.h>

int memstream_init(memstream_t *stream) {
  memset(stream, 0, sizeof(memstream_t));

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

void memstream_flush(memstream_t *stream) {
  if(stream->m_hdl != NULL) {
    fflush(stream->m_hdl);
  }
}

void memstream_close_hdl(memstream_t *stream) {
  if(stream->m_hdl != NULL) {
    fclose(stream->m_hdl);
    stream->m_hdl = NULL;
  }
}

void memstream_free_buf(memstream_t *stream) {
  if(stream->m_buf != NULL) {
    free(stream->m_buf);
    stream->m_buf = NULL;
    stream->m_len = 0;
  }
}

void memstream_free(memstream_t *stream) {
  memstream_close_hdl(stream);
  memstream_free_buf(stream);
}

slice_t memstream_slice(memstream_t *stream) {
  memstream_flush(stream);
  slice_t slice = { stream->m_len, stream->m_buf };
  return slice;
}
