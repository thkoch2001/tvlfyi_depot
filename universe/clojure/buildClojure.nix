{ universe ? import <universe> {}, ... }:

universe.nix.buildClojure.program {
  name = "test";
  deps = with universe.third_party.clojure; [

  ];
  srcs = [
    ./main.clj
  ]
}
