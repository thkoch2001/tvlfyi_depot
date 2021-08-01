{ depot, pkgs, ... }:

let
  src = pkgs.applyPatches {
    name = "closure-html-source";
    src = pkgs.fetchFromGitHub {
      owner = "bluelisp";
      repo = "closure-html";
      rev = "fee42604ae36884d2f7c5e8ffc3441fdb8ec77b7"; # 2017-04-19
      sha256 = "105vm29qnxh6zj3rh4jwpm8dyp3b9bsva64c8a78cr270p28d032";
    };

    patches = [
      # delete unexported and unused double defun in sgml-dtd.lisp
      # which reference undefined CL-USER:*HTML-DTD* (!) which
      # unlike CLOSURE-HTML:*HTML-DTD* is not involved in the
      # packages operation.
      ./no-double-defun.patch
      # Patches html-parser.lisp to look for the distributed
      # dtd files and catalog in this source derivations out
      # path in the nix store instead of the same directory
      # relatively to the (built) system.
      ./dtds-from-store.patch
    ];

    postPatch = ''
      # Inject file which defines CLOSURE-HTML:*HTML-DTD*
      # early in the package's build since SBCL otherwise
      # fails due to the undefined variable. Need to inject
      # this via postPatch since using a nix file results
      # in failure to look up the file's true name which
      # is done for … reasons, apparently.
      cat > src/define-html-dtd.lisp << EOF
      (in-package :closure-html)
      (defvar *html-dtd*)
      EOF

      # Substitute reference to @out@ of this source
      # directory in this patched file.
      substituteAllInPlace src/parse/html-parser.lisp
    '';
  };

  getSrcs = builtins.map (p: "${src}/${p}");
in

depot.nix.buildLisp.library {
  name = "closure-html";

  srcs = getSrcs [
    "closure-html.asd"
    "src/defpack.lisp"
    "src/define-html-dtd.lisp"
    "src/glisp/util.lisp"
    "src/util/clex.lisp"
    "src/util/lalr.lisp"
    "src/net/mime.lisp"
    "src/parse/pt.lisp"
    "src/parse/sgml-dtd.lisp"
    "src/parse/sgml-parse.lisp"
    "src/parse/html-parser.lisp"
    "src/parse/lhtml.lisp"
    "src/parse/unparse.lisp"
    "src/parse/documentation.lisp"
  ];

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
    depot.third_party.lisp.flexi-streams
    depot.third_party.lisp.closure-common
  ];
}
