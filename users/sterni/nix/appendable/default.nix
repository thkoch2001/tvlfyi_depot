{ depot, ... }:

let

  inherit (depot.users.sterni.nix)
    fun
    ;

  goodAttrByPath = p: v:
    builtins.foldl' (set: attr: let res = set."${attr}"; in res) v p;

  seqAttrs = attrs:
    let
      evalAttrs = builtins.foldl'
        (x: a: builtins.seq x attrs."${a}")
        null
        (builtins.attrNames attrs);
    in builtins.seq evalAttrs attrs;

  seqList = list:
    builtins.seq (builtins.foldl' builtins.seq null list) list;

  empty = {};

  size = a: a.size or 0;

  maxNodeSize = 10000;
  nodeSize = a: a.nodeSize or 0;

  append = a: v:
    if nodeSize a < maxNodeSize
    then seqAttrs (a // {
      size = size a + 1;
      nodeSize = nodeSize a + 1;
      value = seqList ((a.value or []) ++ [ v ]);
    }) else let
      new = {
        size = 1 + size a;
        nodeSize = 1;
        previous = a;
        value = seqList [ v ];
      };
    in seqAttrs new;

  get = null;

  toList = a:
    let
      nodeCount = 

in {
  inherit
    empty
    append
    toList
    ;
}
