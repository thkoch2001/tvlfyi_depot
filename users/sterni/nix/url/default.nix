{ depot, lib, ... }:

let

  inherit (depot.users.sterni.nix)
    char
    int
    string
    flow
    ;

  reserved = c: builtins.elem c [
    "!"
    "#"
    "$"
    "&"
    "'"
    "("
    ")"
    "*"
    "+"
    ","
    "/"
    ":"
    ";"
    "="
    "?"
    "@"
    "["
    "]"
  ];

  unreserved = c: char.asciiAlphaNum c
    || builtins.elem c [ "-" "_" "." "~" ];

  percentEncode = c:
    if unreserved c
    then c
    else "%" + (string.fit
      {
        width = 2;
        char = "0";
        side = "left";
      }
      (int.toHex (char.ord c)));

  encode = { leaveReserved ? false }: s:
    let
      chars = lib.stringToCharacters s;
      tr = c:
        if leaveReserved && reserved c
        then c
        else percentEncode c;
    in
    lib.concatStrings (builtins.map tr chars);

  decode = s:
    let
      tokens = builtins.split "%" s;
      decodeStep =
        { result ? ""
        , inPercent ? false
        }: s:
        flow.cond [
          [
            (builtins.isList s)
            {
              inherit result;
              inPercent = true;
            }
          ]
          [
            inPercent
            {
              inPercent = false;
              # first two characters came after an %
              # the rest is the string until the next %
              result = result
                + char.chr (int.fromHex (string.take 2 s))
                + (string.drop 2 s);
            }
          ]
          [
            (!inPercent)
            {
              result = result + s;
            }
          ]
        ];

    in
    (builtins.foldl' decodeStep { } tokens).result;

in
{
  inherit
    encode
    decode
    ;
}
