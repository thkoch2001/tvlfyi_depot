{ ... }:

rec {
  inherit (builtins)
    mul
    div
    add
    sub
    ;

  sign = i: if i < 0 then -1 else 1;
  abs = i: if i < 0 then -i else i;

  inRange = a: b: x: x >= a && x <= b;

  sum = builtins.foldl' (a: b: a + b) 0;
}
