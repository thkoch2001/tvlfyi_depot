{ depot, ... }:

let

  inherit (depot.users.sterni.nix.flow)
    cond
    ;

  # TODO(sterni): make safe
  elemAt = i: l:
    builtins.elemAt l i;

  elemIndex = element: list:
    let
      go = e: l: i:
        cond [
          [ (builtins.length l == 0) null ]
          [ (builtins.head l == e) i ]
          [ true (go e (builtins.tail l) (i + 1)) ]
        ];
    in
      go element list 0;

in {
  inherit
    elemIndex
    elemAt
    ;
}
