#include <su/char.h>

char dec_digit(short d) {
    return (d + 48);
}

char hex_digit(short h) {
    if(h <= 9) {
        return dec_digit(h);
    } else {
        return (h + 55);
    }
}

short hex_int(char c) {
    if(c >= '0' && c <= '9') {
        return c - '0';
    } else if(c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    } else if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    }
    return -1;
}
