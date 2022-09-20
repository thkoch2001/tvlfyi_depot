[
  (builtins.baseNameOf "")
  (builtins.baseNameOf ./foo.txt)
  (builtins.baseNameOf "/abs/path/to/foo.txt")
  (builtins.baseNameOf "./rel/dir/..")
  (builtins.baseNameOf "/abs/dir/..")
  # strange inputs
  (builtins.baseNameOf "foo.txt/")
  (builtins.baseNameOf "./foo.txt/")
  (builtins.baseNameOf "/foo.txt/")
  (builtins.baseNameOf "foo/////")
  (builtins.baseNameOf "foo/////bar")
  (builtins.baseNameOf { __toString = _: { __toString = _: { __toString = _: /not/a/string; }; }; })
]
