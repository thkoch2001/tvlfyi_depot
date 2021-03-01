#ifndef SU_CHAR_H
#define SU_CHAR_H

/// Map integers 0 - 9 to `'0'` - `'9'`
char su_to_dec_digit(short);

/// Map integers 0 - 15 to `'0'` - `'F'`.
/// Hexadecimal digits are always uppercase.
char su_to_hex_digit(short);

/// Map `'0'` - `'F'` to integers 0 - 16
short su_from_hex_digit(char);

#endif
