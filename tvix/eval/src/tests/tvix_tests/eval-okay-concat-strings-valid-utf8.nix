# This test checks valid utf-8 strings outside the BMP
let
  u_0xf0  = "ð";
  u_0x9f  = "Ÿ";
  u_0x92  = "’";
  u_0xa9  = "©";
  valid   = u_0xf0 + u_0x9f + u_0x92 + u_0xa9;
in
  [ valid (builtins.stringLength valid) ]
