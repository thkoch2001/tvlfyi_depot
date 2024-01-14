map (e: (builtins.tryEval e).success) (map builtins.fromJSON [
  (builtins.throw "a")
  {
    a = builtins.throw "attribute a";
  }
  {
    a.b.c.d.e.f.g.h.i = builtins.throw "deep i";
  }
  {
    x = 32;
    y = builtins.throw "second argument";
  }
  # FIXME(raitobezarius): we would like to test coercions, i.e. `toFile` and `derivation` containing throwables.
])
