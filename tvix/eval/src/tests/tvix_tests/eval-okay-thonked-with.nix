# Creates a `with` across multiple thonk boundaries.

let
  set = {
    a = with { b = 42; }; b;
  };
in set.a
