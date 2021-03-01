#include <stdio.h>
#include <su/json/render.h>

#define SU_JSON_NUM_BUF_SIZE 512

void su_json_putc(su_json_renderer_t *ctx, char c) {
  fputc(c, ctx->out);
}

void su_json_init(su_json_renderer_t *ctx, FILE *out) {
  ctx->out = out;
  ctx->need_comma = false;
}

void su_json_object(su_json_renderer_t *ctx) {
  if(ctx->need_comma) {
    su_json_putc(ctx, ',');
  }
  su_json_putc(ctx, '{');
  ctx->need_comma = false;
}

void su_json_object_end(su_json_renderer_t *ctx) {
  su_json_putc(ctx, '}');
  ctx->need_comma = true;
}

void su_json_bind_len(su_json_renderer_t *ctx, const char *b, size_t len) {
  su_json_string_len(ctx, b, len);
  ctx->need_comma = false;

  su_json_putc(ctx, ':');
}

void su_json_bind(su_json_renderer_t *ctx, slice_t slice) {
  su_json_bind_len(ctx, slice.slice_buf, slice.slice_len);
}

void su_json_array(su_json_renderer_t *ctx) {
  if(ctx->need_comma) {
    su_json_putc(ctx, ',');
  }
  su_json_putc(ctx, '[');
  ctx->need_comma = false;
}

void su_json_array_end(su_json_renderer_t *ctx) {
  su_json_putc(ctx, ']');
  ctx->need_comma = true;
}

void su_json_string_len(su_json_renderer_t *ctx, const char *s, size_t len) {
  if(ctx->need_comma) {
    su_json_putc(ctx, ',');
  }

  ctx->need_comma = true;

  su_json_putc(ctx, '"');

  for(size_t i = 0; i < len; i++) {
    char c;
    bool escape = false;

    switch(s[i]) {
      case '\t':
        escape = true;
        c = 't';
        break;
      case '\f':
        escape = true;
        c = 'f';
        break;
      case '\r':
        escape = true;
        c = 'r';
        break;
      case '\n':
        escape = true;
        c = 'n';
        break;
      case '\\':
      case '"':
        escape = true;
        // fall through
      default:
        c = s[i];
        break;
    }

    if(escape) {
      su_json_putc(ctx, '\\');
    }

    su_json_putc(ctx, c);
  }

  su_json_putc(ctx, '"');
}

void su_json_string(su_json_renderer_t *ctx, slice_t slice) {
  su_json_string_len(ctx, slice.slice_buf, slice.slice_len);
}

void su_json_null(su_json_renderer_t *ctx) {
  if(ctx->need_comma) {
    su_json_putc(ctx, ',');
  }

  ctx->need_comma = true;

  (void) fwrite("null", 1, sizeof("null") - 1, ctx->out);
}

void su_json_bool(su_json_renderer_t *ctx, bool b) {
  if(ctx->need_comma) {
    su_json_putc(ctx, ',');
  }

  ctx->need_comma = true;

  size_t len;
  char *s;

  if(b) {
    s = "true";
    len = sizeof("true") - 1;
  } else {
    s = "false";
    len = sizeof("false") - 1;
  }

  (void) fwrite(s, 1, len, ctx->out);
}

// generics for C 🤡
#define SU_INT_FUN(name, type, sign) \
  void name(su_json_renderer_t *ctx, type u) {                 \
    if(ctx->need_comma) {                                      \
      su_json_putc(ctx, ',');                                  \
    }                                                          \
                                                               \
    ctx->need_comma = true;                                    \
                                                               \
    char buf[SU_JSON_NUM_BUF_SIZE];                            \
    type d;                                                    \
    size_t len = 0;                                            \
    char *start = buf + SU_JSON_NUM_BUF_SIZE;                  \
    bool add_sign = false;                                     \
                                                               \
    if(sign && u < 0) {                                        \
      add_sign = true;                                         \
    }                                                          \
                                                               \
    do {                                                       \
      start--;                                                 \
      len++;                                                   \
      d = u % 10;                                              \
      if(add_sign) {                                           \
        d = -d;                                                \
      }                                                        \
      u = u / 10;                                              \
      *start = d + 48;                                         \
    } while(u != 0 && len <= SU_JSON_NUM_BUF_SIZE);            \
                                                               \
    if(add_sign) {                                             \
      su_json_putc(ctx, '-');                                  \
    }                                                          \
                                                               \
    (void) fwrite(start, sizeof(char), len, ctx->out);         \
  }

SU_INT_FUN(su_json_uint, unsigned int, false)
SU_INT_FUN(su_json_int, int, true)

SU_INT_FUN(su_json_long_long, long long int, true)
SU_INT_FUN(su_json_long, long int, true)
SU_INT_FUN(su_json_ulong, unsigned long int, false)
SU_INT_FUN(su_json_ulong_long, unsigned long long int, false)

SU_INT_FUN(su_json_int8, int8_t, true)
SU_INT_FUN(su_json_int16, int16_t, true)
SU_INT_FUN(su_json_int32, int32_t, true)
SU_INT_FUN(su_json_int64, int64_t, true)

SU_INT_FUN(su_json_uint8, uint8_t, false)
SU_INT_FUN(su_json_uint16, uint16_t, false)
SU_INT_FUN(su_json_uint32, uint32_t, false)
SU_INT_FUN(su_json_uint64, uint64_t, false)
