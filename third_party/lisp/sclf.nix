{ depot, pkgs, ... }:

let
  src = pkgs.applyPatches {
    name = "sclf-source";

    src = pkgs.fetchzip {
      url = "http://wcp.sdf-eu.org/software/sclf-20150207T213551.tbz";
      sha256 = "09qb9xcrpn9jqx9mf6h1yjzhyk6l5p2lbrlnmqfih0qbj9qcypfg";
    };

    patches = [];

    # SBCL Package Locking doesn't like its use and since it is a constant,
    # we can just replace it…
    postPatch = ''
      substituteInPlace directory.lisp \
        --replace "sb-impl::unix-to-universal-time" "2208988800"
    '';
  };

  getSrcs = builtins.map (p: "${src}/${p}");
in

depot.nix.buildLisp.library {
  name = "sclf";

  deps = [
    (depot.nix.buildLisp.bundled "sb-posix")
    (depot.nix.buildLisp.bundled "asdf")
  ];

  srcs = getSrcs [
    "sclf.asd"
    "package.lisp"
    "sclf.lisp"
    "sysproc.lisp"
    "lazy.lisp"
    "directory.lisp"
    "time.lisp"
    "serial.lisp"
    "mp/sbcl.lisp"
  ];
}
