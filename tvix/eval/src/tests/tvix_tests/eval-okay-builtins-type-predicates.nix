let
  # apply is thonked, so we can create a thonked value using the identity function
  thonk = x: x;
in
[
  (builtins.isAttrs { bar = throw "baz"; })
  (builtins.isAttrs (thonk { foo = 13; }))
  (builtins.isAttrs (thonk 123))
  (builtins.isBool true)
  (builtins.isBool (thonk false))
  (builtins.isBool (thonk "lol"))
  (builtins.isFloat 1.2)
  (builtins.isFloat (thonk (1 * 1.0)))
  (builtins.isFloat 1)
  (builtins.isFunction thonk)
  (builtins.isFunction (thonk thonk))
  (builtins.isFunction {})
  (builtins.isInt 1)
  (builtins.isInt (thonk 42))
  (builtins.isInt 1.0)
  (builtins.isList [ (throw "oh no") (abort "it's over") ])
  (builtins.isList (thonk [ 21 21 ]))
  (builtins.isList (thonk {}))
  (builtins.isNull null)
  (builtins.isNull (thonk null))
  (builtins.isNull 42)
  (builtins.isPath ./relative)
  (builtins.isPath (thonk /absolute))
  (builtins.isPath "/not/a/path")
  (builtins.isString "simple")
  (builtins.isString "${{ outPath = "coerced"; }}")
  (builtins.isString "hello ${"interpolation"}")
  (builtins.isString true)
]
