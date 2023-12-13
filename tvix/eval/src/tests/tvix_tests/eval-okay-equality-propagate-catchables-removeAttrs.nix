(builtins.tryEval (builtins.removeAttrs (throw "fred") ["x"])).success
