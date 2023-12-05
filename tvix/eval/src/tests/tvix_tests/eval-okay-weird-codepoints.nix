let
  s = "🕰️";
in
[
  (builtins.stringLength s)
  (builtins.stringLength (builtins.substring 0 1 s))
]
