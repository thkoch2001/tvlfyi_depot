{ depot, lib, ... }:

let

  inherit (depot.users.sterni.nix.char)
    chr
    ord
    ;

  inherit (depot.users.sterni.nix.flow)
    cond
    ;

  stringDrop = n: s:
    builtins.substring n (builtins.stringLength s - n) s;

  charAt = i: s:
    let
      r = builtins.substring i 1 s;
    in if r == "" then null else r;

  charIndex = char: s:
    let
      go = i:
        cond [
          [ (s == "") null ]
          [ (charAt i s == char) i ]
          [ true (go (i + 1)) ]
        ];
    in go 0;

  toChars = lib.stringToCharacters;
  fromChars = lib.concatStrings;

  toBytes = str:
    builtins.map ord (toChars str);

  fromBytes = is: lib.concatMapStrings chr is;

in {
  inherit
    stringDrop
    charAt
    charIndex
    toBytes
    fromBytes
    toChars
    fromChars
    ;
}
