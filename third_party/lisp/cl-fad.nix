# Portable pathname library
{ depot, pkgs, ... }:

with depot.nix;

let
  src = pkgs.fetchFromGitHub {
    owner = "edicl";
    repo = "cl-fad";
    rev = "13cbffe08fc660041359302f4057f8fc20d09402"; # 2021-01-10
    sha256 = "049laj8an6g9bh0m0cn0bxhq313p8qq1h37cil15l66147ad8slc";
  };
in buildLisp.library {
  name = "cl-fad";

  deps = with depot.third_party.lisp; [
    alexandria
    bordeaux-threads
    { sbcl = buildLisp.bundled "sb-posix"; }
  ];

  srcs = map (f: src + ("/" + f)) [ "packages.lisp" ]
    ++ [{ ccl = "${src}/openmcl.lisp"; }] ++ map (f: src + ("/" + f)) [
      "fad.lisp"
      "path.lisp"
      "temporary-files.lisp"
    ];
}
