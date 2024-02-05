map (e: (builtins.tryEval e).success) [
  (builtins.removeAttrs (builtins.throw "a") [ "a" ])
  (builtins.removeAttrs { a = {}; } (builtins.throw "b"))
  (builtins.removeAttrs { a = builtins.throw "b"; } [ "a" ])
]
