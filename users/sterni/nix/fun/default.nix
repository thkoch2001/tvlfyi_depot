{ depot, lib, ... }:

let

  # Simple function composition,
  # application is right to left.
  dot = f1: f2:
    (x: f1 (f2 x));

  # Simple function composition,
  # application is left to right.
  pipe = f1: f2:
    (x: f2 (f1 x));

  # Compose a list of functions,
  # application is left to right
  pipeline = fs: x:
    builtins.foldl' (v: f: f v) x fs;

in

{
  inherit (lib)
    fix
    flip
    const
    id
    ;

  inherit
    dot
    pipe
    pipeline
    ;
}
