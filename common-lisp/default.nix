{ depot ? import <depot> {},
  universe ? import <universe> {},
  ...
}:

depot.nix.buildLisp.program {
  name = "unit-testing";

  deps = with universe.third_party.lisp; [
    prove
  ];

  srcs = [
    ./unit-testing.lisp
  ];
}
