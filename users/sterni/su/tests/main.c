#define _POSIX_C_SOURCE 200809L /* open_memstream */
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include <su/check.h>

#include <su/char.h>
#include <su/json/emit.h>
#include <su/memstream.h>
#include <su/s.h>
#include <su/url.h>

#define JQ_BUF_SIZE 1024

check_t chk;

void check_check(void) {
  check_t tmp;

  check_init(&tmp);
  tmp.check_output = fopen("/dev/null", "w");

  check_assert(&chk, "check: check_init", tmp.check_result);

  check_assert(&tmp, "check: should succeed", true);
  check_assert(&chk, "check: check_assert true", tmp.check_result);

  check_assert(&tmp, "check: should fail", false);
  check_assert(&chk, "check: check_assert false", !tmp.check_result);

  fclose(tmp.check_output);
}

void check_char(void) {
  bool dec_res = dec_digit(0) == '0'
    && dec_digit(2) == '2'
    && dec_digit(3) == '3'
    && dec_digit(4) == '4'
    && dec_digit(5) == '5'
    && dec_digit(6) == '6'
    && dec_digit(7) == '7'
    && dec_digit(8) == '8'
    && dec_digit(9) == '9';

  check_assert(&chk, "char: decimal digits", dec_res);

  bool hex_res = hex_digit(0) == '0'
    && hex_digit(2) == '2'
    && hex_digit(3) == '3'
    && hex_digit(4) == '4'
    && hex_digit(5) == '5'
    && hex_digit(6) == '6'
    && hex_digit(7) == '7'
    && hex_digit(8) == '8'
    && hex_digit(9) == '9'
    && hex_digit(10) == 'A'
    && hex_digit(11) == 'B'
    && hex_digit(12) == 'C'
    && hex_digit(13) == 'D'
    && hex_digit(14) == 'E'
    && hex_digit(15) == 'F';

  check_assert(&chk, "char: hexadecimal digits", hex_res);
}

void check_s(void) {
  char *zero = "bar";
  s_t from0 = s_from0(zero);
  check_assert(&chk, "s: s_from0 len", from0.s_len == 3);
  check_assert(&chk, "s: s_from0 cap", from0.s_cap >= 3);

  char *zero_conv = s_to0(&from0);

  check_assert(&chk, "s: s_to0 cmp", zero_conv != NULL
      && strcmp(zero, zero_conv) == 0);

  s_t from_static = s_from_const("foo");
  check_assert(&chk, "s: s_from_static len", from_static.s_len == 3);
  check_assert(&chk, "s: s_from_static cap", from_static.s_cap >= 3);

  check_assert(&chk, "s: slice_eq not equal, same len",
      !slice_eq(s_slice(from0), s_slice(from_static)));

  check_assert(&chk, "s: slice_eq equal to itself",
      slice_eq(s_slice(from0), s_slice(from0)));

  check_assert(&chk, "s: s_append succeeds",
      s_append(&from_static, slice_from_const(" bar")) == 0);
  check_assert(&chk, "s: expected content after append",
      slice_eq(s_slice(from_static), slice_from_const("foo bar")));

  s_free(&from0);
  s_free(&from_static);
  check_assert(&chk, "s: s_free value reset",
      from_static.s_len == 0
      && from_static.s_cap == 0
      && from_static.s_buf == NULL
      && from0.s_len == 0
      && from0.s_cap == 0
      && from0.s_buf == NULL);

  check_assert(&chk, "s: freed is s_empty",
      s_empty(from_static) && s_empty(from0));

  // free empty, double free should not segfault
  s_t empty = s_new(0);
  s_free(&empty);
  s_free(&empty);

  empty = s_from0("lol");
  s_free(&empty);
  s_free(&empty);
}

void check_json_emit(void) {
  struct ej_context ctx;
  memstream_t stream;

  int memstream_res = memstream_init(&stream);

  check_assert(&chk, "memstream: memstream_init succeeds", memstream_res != -1);

  if(memstream_res != -1) {
    ej_init(&ctx, stream.m_hdl);

    ej_object(&ctx);

    ej_bind_const(&ctx, "bools and stuff");
    ej_array(&ctx);
    ej_bool(&ctx, true);
    ej_bool(&ctx, false);
    ej_null(&ctx);
    ej_array_end(&ctx);

    ej_bind_const(&ctx, "strings");
    ej_array(&ctx);
    ej_string(&ctx, slice_from_const("foo\tbar\nbaz"));
    ej_string(&ctx, slice_from_const("form\ffeed"));
    ej_string(&ctx, slice_from_const("🤭 unicode 😳"));
    ej_array_end(&ctx);

    ej_bind_const(&ctx, "objects");
    ej_array(&ctx);
    ej_object(&ctx);
    ej_bind_len(&ctx, "hello", 5);
    ej_string_len(&ctx, "world", 5);
    ej_bind(&ctx, slice_from_const("foo\r\nbar"));
    ej_uint(&ctx, 42);
    ej_object_end(&ctx);
    ej_object(&ctx);
    ej_object_end(&ctx);
    ej_array_end(&ctx);

    ej_bind_const(&ctx, "numbers");
    ej_array(&ctx);
    ej_uint(&ctx, 1312);
    ej_uint(&ctx, 25500001);
    ej_int(&ctx, -12000);
    ej_int(&ctx, 10000);
    ej_long(&ctx, -50000);
    ej_ulong(&ctx, 18340983094);
    ej_long_long(&ctx, 129302193092);
    ej_ulong_long(&ctx, 129302193092);

    ej_int8(&ctx, INT8_MAX);
    ej_int8(&ctx, INT8_MIN);
    ej_int16(&ctx, INT16_MAX);
    ej_int16(&ctx, INT16_MIN);
    ej_int32(&ctx, INT32_MAX);
    ej_int32(&ctx, INT32_MIN);
    ej_int64(&ctx, INT64_MAX);
    ej_int64(&ctx, INT64_MIN);

    ej_uint8(&ctx, UINT8_MAX);
    ej_uint8(&ctx, 0);
    ej_uint16(&ctx, UINT16_MAX);
    ej_uint16(&ctx, 0);
    ej_uint32(&ctx, UINT32_MAX);
    ej_uint32(&ctx, 0);
    ej_uint64(&ctx, UINT64_MAX);
    ej_uint64(&ctx, 0);

    ej_array_end(&ctx);
    ej_object_end(&ctx);

    memstream_flush(&stream);
    bool len_correct = stream.m_len == ctx.written;

    check_assert(&chk, "json/emit: written size matches computed size", len_correct);

    // TODO(sterni): check validity using jq

    int jq_stdin[2];
    int jq_stdout[2];

    if(pipe(jq_stdin) != -1 && pipe(jq_stdout) != -1) {
      int jq_pid = fork();

      if(jq_pid == 0) {
        close(jq_stdout[0]);
        close(jq_stdin[1]);

        if(dup2(jq_stdin[0], STDIN_FILENO) == -1) {
          exit(101);
        }

        if(dup2(jq_stdout[1], STDOUT_FILENO) == -1) {
          exit(101);
        }

        if(dup2(jq_stdout[1], STDERR_FILENO) == -1) {
          exit(101);
        }

        close(jq_stdin[0]);
        close(jq_stdout[1]);

        int res = execlp("jq", "jq", NULL);

        exit(res);
      } else if(jq_pid > 0) {
        close(jq_stdin[0]);
        close(jq_stdout[1]);

        slice_writefd(memstream_slice(&stream), jq_stdin[1]);
        close(jq_stdin[1]);

        char jq_output[JQ_BUF_SIZE];
        ssize_t read_bytes;

        do {
          read_bytes = read(jq_stdout[0], jq_output, JQ_BUF_SIZE);
          fwrite(jq_output, sizeof(char), read_bytes, chk.check_output);
        } while(read_bytes > 0);

        close(jq_stdout[0]);

        int wstatus;
        waitpid(jq_pid, &wstatus, 0);

        check_assert(&chk, "json/emit: jq likes output",
            WIFEXITED(wstatus) && WEXITSTATUS(wstatus) == 0);
      } else {
        perror("couldn't fork to start jq");
      }
    } else {
      perror("couldn't create pipe for jq");
    }

    memstream_free(&stream);
  }
}

void check_memstream(void) {
  memstream_t stream;
  int res = memstream_init(&stream);
  check_assert(&chk, "memstream: memstream_init", res == 0);

  if(res == 0) {
    slice_t test = slice_from_const("Hello, World!");
    slice_write(test, stream.m_hdl);

    check_assert(&chk, "memstream: content",
        slice_eq(memstream_slice(&stream), test));

    memstream_free(&stream);

    check_assert(&chk, "memstream: memstream_free value reset",
        stream.m_len == 0
        && stream.m_hdl == NULL
        && stream.m_buf == NULL);
  }
}

void check_url(void) {
  // test escaping while ignoring reserved characters
  const slice_t urls[] = {
    slice_from_const("ftp://ftp.is.co.za/rfc/rfc1808.txt"),
    slice_from_const("http://www.ietf.org/rfc/rfc2396.txt"),
    slice_from_const("ldap://[2001:db8::7]/c=GB?objectClass?one"),
    slice_from_const("mailto:John.Doe@example.com"),
    slice_from_const("news:comp.infosystems.www.servers.unix"),
    slice_from_const("tel:+1-816-555-1212"),
    slice_from_const("telnet://192.0.2.16:80/"),
    slice_from_const("urn:oasis:names:specification:docbook:dtd:xml:4.1.2"),
  };

  for(size_t i = 0; i < (sizeof(urls) / sizeof(slice_t)); i++) {
    s_t output = urlencode(urls[i], false, false);

    check_assert(&chk, "url: urlencode succeeds", !s_empty(output));

    check_assert(&chk, "url: urlencode w/o reserved escaping (input == output)",
        slice_eq(urls[i], s_slice(output)));

    s_free(&output);
  }

  const char *unreserved = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_~";

  const slice_t encoded[] = {
    slice_from_const("Laguna%20Beach"),
    slice_from_const("Aliens%20%F0%9F%91%BE"),
    slice_from_const(unreserved),
    slice_from_const("%60%21%40%23%24%25%5E%26%2A%28%29%2B%3D%7B%7D%5B%5D%3A%3B%27%5C%7C%3C%3E%2C%3F%2F%20%22"),
  };

  const slice_t decoded[] = {
    slice_from_const("Laguna Beach"),
    slice_from_const("Aliens 👾"),
    slice_from_const(unreserved),
    slice_from_const("`!@#$%^&*()+={}[]:;'\\|<>,?/ \""),
  };

  if(sizeof(encoded) == sizeof(decoded)) {
    for(size_t i = 0; i < (sizeof(encoded) / sizeof(slice_t)); i++) {
      s_t tmp_encoded = urlencode(decoded[i], true, true);
      s_t tmp_decoded = urldecode(encoded[i]);

      check_assert(&chk, "url: urlencode matches expected",
          slice_eq(s_slice(tmp_encoded), encoded[i]));

      check_assert(&chk, "url: urldecode matches expected",
          slice_eq(s_slice(tmp_decoded), decoded[i]));

      s_free(&tmp_encoded);
      s_free(&tmp_decoded);
    }
  }
}

int main(void) {
  check_init(&chk);
  chk.check_name_width = 70;

  check_check();
  check_char();
  check_s();
  check_json_emit();
  check_memstream();
  check_url();

  check_finish(&chk);

  return 0;
}
