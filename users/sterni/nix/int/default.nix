{ depot, lib, ... }:

let

  # TODO(sterni): implement nix.float and figure out which of these
  #               functions can be split out into a common nix.num
  #               library.

  inherit (depot.users.sterni.nix)
    string
    ;

  inherit (builtins)
    bitOr
    bitAnd
    bitXor
    mul
    div
    add
    sub
    ;

  abs = i: if i < 0 then -i else i;

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

  toHex = int:
    let
      go = i:
        if i == 0
        then ""
        else go (bitShiftR i 4)
          + string.charAt (bitAnd i 15) hexdigits;
      sign = lib.optionalString (int < 0) "-";
    in
    if int == 0
    then "0"
    else "${sign}${go (abs int)}";

  fromHexMap = builtins.listToAttrs
    (lib.imap0 (i: c: { name = c; value = i; })
      (lib.stringToCharacters hexdigits));

  fromHex = literal:
    let
      negative = string.charAt 0 literal == "-";
      start = if negative then 1 else 0;
      len = builtins.stringLength literal;
      # reversed list of all digits
      digits = builtins.genList
        (i: string.charAt (len - 1 - i) literal)
        (len - start);
      parsed = builtins.foldl'
        (v: d: {
          val = v.val + (fromHexMap."${d}" * v.mul);
          mul = v.mul * 16;
        })
        { val = 0; mul = 1; }
        digits;
    in
    if negative
    then -parsed.val
    else parsed.val;

  # A nix integer is a 64bit signed integer
  maxBound = 9223372036854775807;

  # fun fact: -9223372036854775808 is the lower bound
  # for a nix integer (as you would expect), but you can't
  # use it as an integer literal or you'll be greeted with:
  # error: invalid integer '9223372036854775808'
  # This is because all int literals when parsing are
  # positive, negative "literals" are positive literals
  # which are preceded by the arithmetric negation operator.
  minBound = -9223372036854775807 - 1;

  odd = x: bitAnd x 1 == 1;
  even = x: bitAnd x 1 == 0;

  # div and mod behave like quot and rem in Haskell,
  # i. e. they truncate towards 0
  mod = a: b: let res = a / b; in a - (res * b);

  inRange = a: b: x: x >= a && x <= b;

in
{
  inherit
    maxBound
    minBound
    abs
    exp
    odd
    even
    add
    sub
    mul
    div
    mod
    bitShiftR
    bitShiftL
    bitOr
    bitAnd
    bitXor
    toHex
    fromHex
    inRange
    ;
}
