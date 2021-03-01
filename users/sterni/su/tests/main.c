#define _POSIX_C_SOURCE 200809L /* open_memstream */
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include <su/check.h>

#include <su/char.h>
#include <su/json/render.h>
#include <su/memstream.h>
#include <su/s.h>
#include <su/time.h>
#include <su/url.h>

#define JQ_BUF_SIZE 1024

su_check_t chk;

void check_check(void) {
  su_check_t tmp;

  su_check_init(&tmp);
  tmp.check_output = fopen("/dev/null", "w");

  su_check_assert(&chk, "check: check_init", tmp.check_result);

  su_check_assert(&tmp, "check: should succeed", true);
  su_check_assert(&chk, "check: check_assert true", tmp.check_result);

  su_check_assert(&tmp, "check: should fail", false);
  su_check_assert(&chk, "check: check_assert false", !tmp.check_result);

  fclose(tmp.check_output);
}

void check_char(void) {
  bool dec_res = su_to_dec_digit(0) == '0'
    && su_to_dec_digit(2) == '2'
    && su_to_dec_digit(3) == '3'
    && su_to_dec_digit(4) == '4'
    && su_to_dec_digit(5) == '5'
    && su_to_dec_digit(6) == '6'
    && su_to_dec_digit(7) == '7'
    && su_to_dec_digit(8) == '8'
    && su_to_dec_digit(9) == '9';

  su_check_assert(&chk, "char: decimal digits", dec_res);

  bool hex_res = su_to_hex_digit(0) == '0'
    && su_to_hex_digit(2) == '2'
    && su_to_hex_digit(3) == '3'
    && su_to_hex_digit(4) == '4'
    && su_to_hex_digit(5) == '5'
    && su_to_hex_digit(6) == '6'
    && su_to_hex_digit(7) == '7'
    && su_to_hex_digit(8) == '8'
    && su_to_hex_digit(9) == '9'
    && su_to_hex_digit(10) == 'A'
    && su_to_hex_digit(11) == 'B'
    && su_to_hex_digit(12) == 'C'
    && su_to_hex_digit(13) == 'D'
    && su_to_hex_digit(14) == 'E'
    && su_to_hex_digit(15) == 'F';

  su_check_assert(&chk, "char: hexadecimal digits", hex_res);
}

void check_s(void) {
  char *zero = "bar";
  s_t from0 = s_from0(zero);
  su_check_assert(&chk, "s: s_from0 len", from0.s_len == 3);
  su_check_assert(&chk, "s: s_from0 cap", from0.s_cap >= 3);

  char *zero_conv = s_to0(&from0);

  su_check_assert(&chk, "s: s_to0 cmp", zero_conv != NULL
      && strcmp(zero, zero_conv) == 0);

  s_t from_static = s_from_const("foo");
  su_check_assert(&chk, "s: s_from_static len", from_static.s_len == 3);
  su_check_assert(&chk, "s: s_from_static cap", from_static.s_cap >= 3);

  su_check_assert(&chk, "s: slice_eq not equal, same len",
      !slice_eq(s_slice(from0), s_slice(from_static)));

  su_check_assert(&chk, "s: slice_eq equal to itself",
      slice_eq(s_slice(from0), s_slice(from0)));

  su_check_assert(&chk, "s: s_append succeeds",
      s_append(&from_static, slice_from_const(" bar")) == 0);
  su_check_assert(&chk, "s: expected content after append",
      slice_eq(s_slice(from_static), slice_from_const("foo bar")));

  s_free(&from0);
  s_free(&from_static);
  su_check_assert(&chk, "s: s_free value reset",
      from_static.s_len == 0
      && from_static.s_cap == 0
      && from_static.s_buf == NULL
      && from0.s_len == 0
      && from0.s_cap == 0
      && from0.s_buf == NULL);

  su_check_assert(&chk, "s: freed is s_empty",
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
  su_json_renderer_t ctx;
  su_memstream_t stream;

  int memstream_res = su_memstream_init(&stream);

  su_check_assert(&chk, "memstream: memstream_init succeeds", memstream_res != -1);

  if(memstream_res != -1) {
    su_json_init(&ctx, stream.m_hdl);

    su_json_object(&ctx);

    su_json_bind_const(&ctx, "bools and stuff");
    su_json_array(&ctx);
    su_json_bool(&ctx, true);
    su_json_bool(&ctx, false);
    su_json_null(&ctx);
    su_json_array_end(&ctx);

    su_json_bind_const(&ctx, "strings");
    su_json_array(&ctx);
    su_json_string(&ctx, slice_from_const("foo\tbar\nbaz"));
    su_json_string(&ctx, slice_from_const("form\ffeed"));
    su_json_string(&ctx, slice_from_const("🤭 unicode 😳"));
    su_json_array_end(&ctx);

    su_json_bind_const(&ctx, "objects");
    su_json_array(&ctx);
    su_json_object(&ctx);
    su_json_bind_len(&ctx, "hello", 5);
    su_json_string_len(&ctx, "world", 5);
    su_json_bind(&ctx, slice_from_const("foo\r\nbar"));
    su_json_uint(&ctx, 42);
    su_json_object_end(&ctx);
    su_json_object(&ctx);
    su_json_object_end(&ctx);
    su_json_array_end(&ctx);

    su_json_bind_const(&ctx, "numbers");
    su_json_array(&ctx);
    su_json_uint(&ctx, 1312);
    su_json_uint(&ctx, 25500001);
    su_json_int(&ctx, -12000);
    su_json_int(&ctx, 10000);
    su_json_long(&ctx, -50000);
    su_json_ulong(&ctx, 18340983094);
    su_json_long_long(&ctx, 129302193092);
    su_json_ulong_long(&ctx, 129302193092);

    su_json_int8(&ctx, INT8_MAX);
    su_json_int8(&ctx, INT8_MIN);
    su_json_int16(&ctx, INT16_MAX);
    su_json_int16(&ctx, INT16_MIN);
    su_json_int32(&ctx, INT32_MAX);
    su_json_int32(&ctx, INT32_MIN);
    su_json_int64(&ctx, INT64_MAX);
    su_json_int64(&ctx, INT64_MIN);

    su_json_uint8(&ctx, UINT8_MAX);
    su_json_uint8(&ctx, 0);
    su_json_uint16(&ctx, UINT16_MAX);
    su_json_uint16(&ctx, 0);
    su_json_uint32(&ctx, UINT32_MAX);
    su_json_uint32(&ctx, 0);
    su_json_uint64(&ctx, UINT64_MAX);
    su_json_uint64(&ctx, 0);

    su_json_array_end(&ctx);
    su_json_object_end(&ctx);

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

        slice_writefd(su_memstream_slice(&stream), jq_stdin[1]);
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

        su_check_assert(&chk, "json/emit: jq likes output",
            WIFEXITED(wstatus) && WEXITSTATUS(wstatus) == 0);
      } else {
        perror("couldn't fork to start jq");
      }
    } else {
      perror("couldn't create pipe for jq");
    }

    su_memstream_free(&stream);
  }
}

void check_memstream(void) {
  su_memstream_t stream;
  int res = su_memstream_init(&stream);
  su_check_assert(&chk, "memstream: memstream_init", res == 0);

  if(res == 0) {
    slice_t test = slice_from_const("Hello, World!");
    slice_write(test, stream.m_hdl);

    su_check_assert(&chk, "memstream: content",
        slice_eq(su_memstream_slice(&stream), test));

    su_memstream_free(&stream);

    su_check_assert(&chk, "memstream: memstream_free value reset",
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
    s_t output = su_urlencode(urls[i], false, false);

    su_check_assert(&chk, "url: urlencode succeeds", !s_empty(output));

    su_check_assert(&chk, "url: urlencode w/o reserved escaping (input == output)",
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
      s_t tmp_encoded = su_urlencode(decoded[i], true, true);
      s_t tmp_decoded = su_urldecode(encoded[i]);

      su_check_assert(&chk, "url: urlencode w/ reserved escaping",
          slice_eq(s_slice(tmp_encoded), encoded[i]));

      su_check_assert(&chk, "url: urldecode",
          slice_eq(s_slice(tmp_decoded), decoded[i]));

      s_free(&tmp_encoded);
      s_free(&tmp_decoded);
    }
  }
}

void check_time(void) {
  su_dot_time_t time = { 1560982380, 0 };

  const int offsets[] = { -4, 0, 2 };

  const slice_t expected_dot_datetime[] = {
    slice_from_const("2019-06-19T22·13-04"),
    slice_from_const("2019-06-19T22·13+00"),
    slice_from_const("2019-06-19T22·13+02"),
  };

  const slice_t expected_rfc3339 =
    slice_from_const("2019-06-19T22:13:00Z");

  size_t len = sizeof(expected_dot_datetime) / sizeof(slice_t);

  for(size_t i = 0; i < len; i++) {
    time.dt_offset = offsets[i];
    s_t rendered = su_render_dot_datetime(time);

    su_check_assert(&chk, "time: su_render_dot_datetime",
        slice_eq(expected_dot_datetime[i], s_slice(rendered)));

    s_t rfc3339_rendered = su_render_rfc3339_utc(time);

    su_check_assert(&chk, "time: su_render_rfc3339_utc",
        slice_eq(expected_rfc3339, s_slice(rfc3339_rendered)));

    s_free(&rendered);
    s_free(&rfc3339_rendered);
  }
}

int main(void) {
  su_check_init(&chk);
  chk.check_name_width = 70;

  check_check();
  check_char();
  check_s();
  check_json_emit();
  check_memstream();
  check_time();
  check_url();

  // TODO(sterni): time, xml

  su_check_finish(&chk);

  return 0;
}
