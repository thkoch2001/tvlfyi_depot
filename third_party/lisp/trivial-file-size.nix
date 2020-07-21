{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "ruricolist";
    repo = "trivial-file-size";
    rev = "ded4e88f20694eb04c7843d4594cc97489b47753";
    sha256 = "1b9d0bfl5s48i43m14frkhd5jhjvqlr4m4cgjh26f2kbqxnibhi3";
  };

in depot.nix.buildLisp.library {
  name = "trivial-file-size";

  deps = with depot.third_party.lisp; [
    uiop
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "trivial-file-size.lisp"
  ];
}
