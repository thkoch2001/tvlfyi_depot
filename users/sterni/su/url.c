#include <su/char.h>
#include <su/s.h>
#include <su/url.h>

s_t su_urlencode(slice_t input, bool escape_reserved, bool escape_slash) {
    s_t output = s_new(input.slice_len);

    if(!s_has_cap(output, 1)) {
        s_free(&output);
        return output;
    }

    char output_size = 0;

    for(s_int i = 0; i < input.slice_len; i++) {
        char c = *(input.slice_buf + i);
        bool needs_escape;
        switch(c) {
            // may be useful to treat specially.
            case '/':
                needs_escape = escape_slash;
                break;
            // reserved characters
            case '!': case '#': case '$': case '&': case '\'':
            case '(': case ')': case '*': case '+': case ',': case ':':
            case ';': case '=': case '?': case '@': case '[': case ']':
                needs_escape = escape_reserved;
                break;
            // unreserved characters: never to escape
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
            case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
            case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
            case 's': case 't': case 'u': case 'v': case 'w': case 'x':
            case 'y': case 'z':
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
            case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
            case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
            case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
            case 'Y': case 'Z':
            case '0': case '1': case '2': case '3': case '4': case '5':
            case '6': case '7': case '8': case '9':
            case '.': case '-': case '_': case '~':
                needs_escape = 0;
                break;
            default:
                needs_escape = 1;
        }

        int necessary_space = needs_escape ? 3 : 1;

        if(s_cap_set(&output, output_size + necessary_space) != 0) {
            s_free(&output); // make empty
            return output;
        }

        if(needs_escape) {
            s_append_char(&output, '%');
            s_append_char(&output, su_to_hex_digit((c & 0xf0) >> 4));
            s_append_char(&output, su_to_hex_digit(c & 0x0f));
        } else {
            s_append_char(&output, c);
        }

        output_size += necessary_space;
    }

    return output;
}

s_t su_urldecode(slice_t input) {
    s_t output = s_new(input.slice_len);
    s_int pos = 0;

    if(!s_has_cap(output, 1)) {
        s_free(&output);
        return output;
    }

    while(pos < input.slice_len) {
        char c = input.slice_buf[pos];

        if(c == '%') {
            if(pos + 2 < input.slice_len) {
                short a = su_from_hex_digit(input.slice_buf[++pos]);
                short b = su_from_hex_digit(input.slice_buf[++pos]);
                s_append_char(&output, (a << 4) + b);
            } else {
                s_free(&output);
                return output;
            }
        } else if(c == '+') {
            s_append_char(&output, ' ');
        } else {
            s_append_char(&output, c);
        }
        pos++;
    }

    return output;
}
