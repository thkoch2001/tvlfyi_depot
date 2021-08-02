{ depot, pkgs, ... }:

let
  src = pkgs.fetchzip {
    url = "http://wcp.sdf-eu.org/software/npg-20150517T144652.tbz";
    sha256 = "1hvm7fx4zjnb9c2km6pvlziih4sjkf4qzbdpqxph0yz6gdybw05c";
  };

  getSrcs = builtins.map (p: "${src}/${p}");
in

depot.nix.buildLisp.library {
  name = "npg";

  srcs = getSrcs [
    "npg.asd"
    "src/package.lisp"
    "src/common.lisp"
    "src/define.lisp"
    "src/parser.lisp"
  ];

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
  ];
}
