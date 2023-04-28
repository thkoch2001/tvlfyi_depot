rec {
  foo = a: b: a + b;
  /**The function bar*/
  bar = foo 1;
}.bar