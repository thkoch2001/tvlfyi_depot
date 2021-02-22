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
    if count == 0
    then bit
    else div (bitShiftR bit (count - 1)) 2;

  bitShiftL = bit: count:
    if count == 0
    then bit
    else 2 * (bitShiftL bit (count - 1));

  hexdigits = "0123456789ABCDEF";

  toHex = i:
    let
      this = string.charAt (bitAnd i 15) hexdigits;
    in if i == 0 then "" else toHex (bitShiftR i 4) + this;

  # A nix integer is a 64bit signed integer
  maxBound = 9223372036854775807;

  # fun fact: you -9223372036854775808 is the lower bound
  # for a nix integer (as you would expect), but you can't
  # use it as an integer literal or you'll be greeted with:
  # error: invalid integer '9223372036854775808'
  minBound = -9223372036854775807 - 1;

in {
  inherit
    maxBound
    minBound
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
