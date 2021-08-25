{ lib, ... }:

let
  escapeMinimal = builtins.replaceStrings
    [ "<"    ">"    "&"     "\""     "'"      ]
    [ "&lt;" "&gt;" "&amp;" "&quot;" "&#039;" ];

  renderTag = tag: attrs: content:
    let
      attrs' = lib.concatMapStrings (n:
        " ${escapeMinimal n}=\"${escapeMinimal attrs.${n}}\""
      ) (builtins.attrNames attrs);
      content' =
        if builtins.isList content
        then lib.concatStrings content
        else content;
    in
      if content == null
      then "<${tag}${attrs'}/>"
      else "<${tag}${attrs'}>${content'}</${tag}>";

  __findFile = _: renderTag;

in {
  inherit __findFile escapeMinimal;

  str = escapeMinimal;
}
