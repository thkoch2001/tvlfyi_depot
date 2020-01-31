{ briefcase ? import <briefcase> {}, ... }:

briefcase.nix.buildClojure.program {
  name = "test";
  deps = with briefcase.third_party.clojure; [

  ];
  srcs = [
    ./main.clj
  ]
}
