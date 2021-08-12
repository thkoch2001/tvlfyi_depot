{ depot, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/trivial-features/trivial-features.git";
  rev = "f6e8dd7268ae0137dbde4be469101a7f735f6416"; # 2021-02-28
};
in depot.nix.buildLisp.library {
  name = "trivial-features";
  srcs = [
    (src + "/src/tf-sbcl.lisp")
  ];
}
