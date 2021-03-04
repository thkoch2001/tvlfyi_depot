{ depot, lib, ... }:

let

  inherit (depot.users.sterni.nix.char)
    chr
    ord
    ;

  inherit (depot.users.sterni.nix)
    int
    flow
    ;

  take = n: s:
    builtins.substring 0 n s;

  drop = n: s:
    builtins.substring n int.maxBound s;

  charAt = i: s:
    let
      r = builtins.substring i 1 s;
    in if r == "" then null else r;

  charIndex = char: s:
    let
      len = builtins.stringLength s;
      go = i:
        flow.cond [
          [ (i >= len) null ]
          [ (charAt i s == char) i ]
          [ true (go (i + 1)) ]
        ];
    in go 0;

  toChars = lib.stringToCharacters;
  fromChars = lib.concatStrings;

  toBytes = str:
    builtins.map ord (toChars str);

  fromBytes = is: lib.concatMapStrings chr is;

  pad = { left ? 0, right ? 0, char ? " " }: s:
    let
      leftS = fromChars (builtins.genList (_: char) left);
      rightS = fromChars (builtins.genList (_: char) right);
    in "${leftS}${s}${rightS}";

  fit = { char ? " ", width, side ? "left" }: s:
    let
      diff = width - builtins.stringLength s;
    in
      if diff <= 0
      then s
      else pad { inherit char; "${side}" = diff; } s;

  # pattern matching for strings only
  match = val: matcher: matcher."${val}";

in {
  inherit
    take
    drop
    charAt
    charIndex
    toBytes
    fromBytes
    toChars
    fromChars
    pad
    fit
    match
    ;
}
