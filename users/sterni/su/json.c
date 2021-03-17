#include <stdio.h>
#include <su/json/emit.h>

void ej_putc(struct ej_context *ctx, char c) {
  fputc(c, ctx->out);
  ctx->written++;
}

void ej_init(struct ej_context *ctx, FILE *out) {
  ctx->out = out;
  ctx->need_comma = false;
  ctx->written = 0;
}

void ej_object(struct ej_context *ctx) {
  if(ctx->need_comma) {
    ej_putc(ctx, ',');
  }
  ej_putc(ctx, '{');
  ctx->need_comma = false;
}

void ej_object_end(struct ej_context *ctx) {
  ej_putc(ctx, '}');
  ctx->need_comma = true;
}

void ej_bind_len(ej_context_t *ctx, const char *b, size_t len) {
  ej_string_len(ctx, b, len);
  ctx->need_comma = false;

  ej_putc(ctx, ':');
}

void ej_bind(ej_context_t *ctx, slice_t slice) {
  ej_bind_len(ctx, slice.slice_buf, slice.slice_len);
}

void ej_array(struct ej_context *ctx) {
  if(ctx->need_comma) {
    ej_putc(ctx, ',');
  }
  ej_putc(ctx, '[');
  ctx->need_comma = false;
}

void ej_array_end(struct ej_context *ctx) {
  ej_putc(ctx, ']');
  ctx->need_comma = true;
}

void ej_string_len(struct ej_context *ctx, const char *s, size_t len) {
  if(ctx->need_comma) {
    ej_putc(ctx, ',');
  }

  ctx->need_comma = true;

  ej_putc(ctx, '"');

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
      ej_putc(ctx, '\\');
    }

    ej_putc(ctx, c);
  }

  ej_putc(ctx, '"');
}

void ej_string(ej_context_t *ctx, slice_t slice) {
  ej_string_len(ctx, slice.slice_buf, slice.slice_len);
}

void ej_null(struct ej_context *ctx) {
  if(ctx->need_comma) {
    ej_putc(ctx, ',');
  }

  ctx->need_comma = true;

  size_t wsize = fwrite("null", 1, sizeof("null") - 1, ctx->out);
  ctx->written += wsize;
}

void ej_bool(struct ej_context *ctx, bool b) {
  if(ctx->need_comma) {
    ej_putc(ctx, ',');
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

  size_t wsize = fwrite(s, 1, len, ctx->out);
  ctx->written += wsize;
}

// generics for C 🤡
#define EJ_INT_FUN(name, type, sign) \
  void name(struct ej_context *ctx, type u) {                  \
    if(ctx->need_comma) {                                      \
      ej_putc(ctx, ',');                                       \
    }                                                          \
                                                               \
    ctx->need_comma = true;                                    \
                                                               \
    char buf[EJ_INT_BUF_SIZE];                                 \
    type d;                                                    \
    size_t len = 0;                                            \
    char *start = buf + EJ_INT_BUF_SIZE;                       \
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
    } while(u != 0 && len <= EJ_INT_BUF_SIZE);                 \
                                                               \
    if(add_sign) {                                             \
      ej_putc(ctx, '-');                                       \
    }                                                          \
                                                               \
    size_t wsize = fwrite(start, sizeof(char), len, ctx->out); \
    ctx->written += wsize;                                     \
  }

EJ_INT_FUN(ej_uint, unsigned int, false)
EJ_INT_FUN(ej_int, int, true)

EJ_INT_FUN(ej_long_long, long long int, true)
EJ_INT_FUN(ej_long, long int, true)
EJ_INT_FUN(ej_ulong, unsigned long int, false)
EJ_INT_FUN(ej_ulong_long, unsigned long long int, false)

EJ_INT_FUN(ej_int8, int8_t, true)
EJ_INT_FUN(ej_int16, int16_t, true)
EJ_INT_FUN(ej_int32, int32_t, true)
EJ_INT_FUN(ej_int64, int64_t, true)

EJ_INT_FUN(ej_uint8, uint8_t, false)
EJ_INT_FUN(ej_uint16, uint16_t, false)
EJ_INT_FUN(ej_uint32, uint32_t, false)
EJ_INT_FUN(ej_uint64, uint64_t, false)
