{ depot, lib, ... }:

let

  inherit (lib)
    id
    ;

  # Simple function composition,
  # application is right to left.
  rl = f1: f2:
    (x: f1 (f2 x));

  # Compose a list of functions,
  # application is right to left.
  rls = fs:
    builtins.foldl' (fOut: f: lr f fOut) id fs;

  # Simple function composition,
  # application is left to right.
  lr = f1: f2:
    (x: f2 (f1 x));

  # Compose a list of functions,
  # application is left to right
  lrs = x: fs:
    builtins.foldl' (v: f: f v) x fs;

  # Check if a function has an attribute
  # set pattern with an ellipsis as its argument.
  # Warning: cursed function
  hasEllipsis = f:
    builtins.isFunction f &&
    builtins.match ".*<attrspat ellipsis=\"1\">.*"
      (builtins.toXML f) != null;

in

{
  inherit (lib)
    fix
    flip
    const
    ;

  inherit
    id
    rl
    rls
    lr
    lrs
    hasEllipsis
    ;
}
