{ depot, ... }:

let

  inherit (depot.users.sterni.nix)
    string
    ;

  inherit (builtins)
    bitOr
    bitAnd
    bitXor
    div
    ;

  abs = i:
    if i < 0
    then -1 * i
    else i;

  exp = base: pow:
    if pow > 0
    then base * (exp base (pow - 1))
    else if pow < 0
    then 1.0 / exp base (abs pow)
    else 1;

  bitShiftR = bit: count:
    div bit (exp 2 count);

  bitShiftL = bit: count:
    bit * (exp 2 count);

  hexdigits = "0123456789ABCDEF";

  toHex = i:
    let
      this = string.charAt (bitAnd i 15) hexdigits;
    in if i == 0 then "" else toHex (bitShiftR i 4) + this;

in {
  inherit
    abs
    exp
    div
    bitShiftR
    bitShiftL
    bitOr
    bitAnd
    bitXor
    toHex
    ;
}
