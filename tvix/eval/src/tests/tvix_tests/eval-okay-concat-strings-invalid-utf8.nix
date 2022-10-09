# this test checks that invalid utf-8 strings can be used as intermediate values
let
  invalid = '\x01\xff';
in
  [ invalid (builtins.stringLength invalid) ]

