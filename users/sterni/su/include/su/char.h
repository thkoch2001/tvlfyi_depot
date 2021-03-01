#ifndef SU_CHAR_H
#define SU_CHAR_H

/// Map integers 0 - 9 to `'0'` - `'9'`
char dec_digit(short);

/// Map integers 0 - 15 to `'0'` - `'F'`.
/// Hexadecimal digits are always uppercase.
char hex_digit(short);

short hex_int(char);

#endif
