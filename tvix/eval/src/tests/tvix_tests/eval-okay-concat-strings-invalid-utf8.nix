# this test checks that invalid utf-8 strings can be used as intermediate values
let
  u_0x01  = ""; # "\x01"
  u_0xff  = "ÿ";  # "\xff": invalid UTF-8
  invalid = u_0x01 + u_0xff;
in
  [ invalid (builtins.stringLength invalid) ]

