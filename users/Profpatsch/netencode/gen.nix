{ lib }:
let

  netstring = tag: suffix: s:
    "${tag}${toString (builtins.stringLength s)}:${s}${suffix}";

  unit = "u,";

  n1 = b: if b then "n1:1," else "n1:0,";

  n = i: n: "n${toString i}:${toString n},";
  i = i: n: "i${toString i}:${toString n},";

  n3 = n 3;
  n6 = n 6;
  n7 = n 7;

  i3 = i 3;
  i6 = i 6;
  i7 = i 7;

  text = netstring "t" ",";
  binary = netstring "b" ",";

  tag = key: val: netstring "<" "|" key + val;

  concatStrings = builtins.concatStringsSep "";

  record = lokv: netstring "{" "}"
    (concatStrings (map ({ key, val }: tag key val) lokv));

  list = l: netstring "[" "]" (concatStrings l);

  dwim = val:
    let match = {
      "bool" = n1;
      "int" = i6;
      "string" = text;
      "set" = attrs:
        # it could be a derivation, then just return the path
        if attrs.type or "" == "derivation" then text "${attrs}"
        else
          record (lib.mapAttrsToList
            (k: v: {
              key = k;
              val = dwim v;
            })
            attrs);
      "list" = l: list (map dwim l);
    };
    in match.${builtins.typeOf val} val;

in
{
  inherit
    unit
    n1
    n3
    n6
    n7
    i3
    i6
    i7
    text
    binary
    tag
    record
    list
    dwim
    ;
}
