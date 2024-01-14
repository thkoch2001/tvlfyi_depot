map (e: (builtins.tryEval e).success) [
  (builtins.listToAttrs [ { name = builtins.throw "a"; value = "b"; } ])
  (builtins.listToAttrs [ { name = "a"; value = builtins.throw "b"; } ])
  (builtins.listToAttrs [ { } { name = "c"; value = builtins.throw "d"; } ])
  (builtins.listToAttrs [ { } (builtins.throw "e") ])
  (builtins.listToAttrs (builtins.throw "f"))
]
