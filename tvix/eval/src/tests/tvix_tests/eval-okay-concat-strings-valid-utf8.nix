# This test checks valid utf-8 strings outside the BMP
let
  valid   = '\xf0\x9f\x92\xa9';
in
  [ valid (builtins.stringLength valid) ]
