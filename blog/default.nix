{
  depot ? import <depot> {},
  universe ? import <universe> {},
  ...
}:

depot.nix.buildLisp.program {
  name = "server";
  deps = with depot.third_party.lisp; [
    hunchentoot
  ];
  srcs = [
    ./src/server.lisp
  ];
}
