#ifndef SU_EMITJSON_H
#define SU_EMITJSON_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include <su/s.h>

typedef struct su_json_renderer {
  FILE *out;
  bool need_comma;
  size_t written;
} su_json_renderer_t;

void su_json_init(su_json_renderer_t *, FILE *);

void su_json_object(su_json_renderer_t *);
void su_json_object_end(su_json_renderer_t *);

#define su_json_bind_const(ctx, b) \
  su_json_bind_len(ctx, (const char *) b, sizeof(b) - 1)

void su_json_bind(su_json_renderer_t *, slice_t);
void su_json_bind_len(su_json_renderer_t *, const char *, size_t);

void su_json_array(su_json_renderer_t *);
void su_json_array_end(su_json_renderer_t *);

void su_json_string_len(su_json_renderer_t *, const char *, size_t);
void su_json_string(su_json_renderer_t *, slice_t);

void su_json_null(su_json_renderer_t *);

void su_json_bool(su_json_renderer_t *, bool);

void su_json_int(su_json_renderer_t *, int);
void su_json_uint(su_json_renderer_t *, unsigned int);

void su_json_long(su_json_renderer_t *, long int);
void su_json_ulong(su_json_renderer_t *, unsigned long int);
void su_json_long_long(su_json_renderer_t *, long long int);
void su_json_ulong_long(su_json_renderer_t *, unsigned long long int);

void su_json_uint8(su_json_renderer_t *, uint8_t);
void su_json_uint16(su_json_renderer_t *, uint16_t);
void su_json_uint32(su_json_renderer_t *, uint32_t);

/// Note that the int range for uint64_t is greater
/// of the JSON number, so JavaScript-compatible
/// parsers will truncate numbers generated by this
/// function.
void su_json_uint64(su_json_renderer_t *, uint64_t);

void su_json_int8(su_json_renderer_t *, int8_t);
void su_json_int16(su_json_renderer_t *, int16_t);
void su_json_int32(su_json_renderer_t *, int32_t);

/// Note that the int range for int64_t is greater
/// of the JSON number, so JavaScript-compatible
/// parsers will truncate numbers generated by this
/// function.
void su_json_int64(su_json_renderer_t *, int64_t);

#endif
