[
  (builtins.hasAttr "foo" { foo = 1; bar = 2; baz = 3; })
  (builtins.hasAttr "bar" { foo = 1; bar = 2; baz = 3; })
  (builtins.hasAttr "baz" { foo = 1; bar = 2; baz = 3; })
  (builtins.hasAttr "FOO" { foo = 1; bar = 2; baz = 3; })
  (builtins.hasAttr "foo" {})
]
