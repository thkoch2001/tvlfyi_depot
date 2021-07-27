{ depot, pkgs, lib, ... }:

let
  src = pkgs.applyPatches {
    name = "cxml-source";

    src = pkgs.fetchFromGitHub {
      owner = "sharplispers";
      repo = "cxml";
      rev = "8701da08ba4aac30891b8d2005edb018c1d3d796"; # 2020-06-05
      sha256 = "18fls3bx7vmnxfa6qara8fxp316d8kb3izar0kysvqg6l0a45a51";
    };

    patches = [
      # Note: *default-catalog* assumes that we are on a normal distro
      # and the catalogs are installed to /etc. We don't use that
      # presently, so we'll just leave it to downstream users to
      # get the catalogs from the nix store.
      ./catalog-from-store.patch
    ];

    # substitue reference to @out@
    postPatch = ''
      substituteAllInPlace xml/catalog.lisp
    '';
  };

  getSrcs = builtins.map (p: "${src}/${p}");

  # cxml was tested against the 2008 version of the xml conformance test
  # suite, the 2013 version is missing some files it expects
  xml-conformance-test-suite = pkgs.fetchzip {
    url = "https://www.w3.org/XML/Test/xmlts20080827.tar";
    sha256 = "0r3ym07jwnqrd6rh4mb29wlmpf8spy82zi70gmz2vw34cl7jhwln";
  };
in

depot.nix.buildLisp.library {
  name = "cxml";

  srcs = getSrcs [
    "cxml.asd"
    # cxml-xml
    "xml/package.lisp"
    "xml/util.lisp"
    "xml/sax-handler.lisp"
    "xml/xml-name-rune-p.lisp"
    "xml/split-sequence.lisp"
    "xml/xml-parse.lisp"
    "xml/unparse.lisp"
    "xml/xmls-compat.lisp"
    "xml/recoder.lisp"
    "xml/xmlns-normalizer.lisp"
    "xml/space-normalizer.lisp"
    "xml/catalog.lisp"
    "xml/sax-proxy.lisp"
    "xml/atdoc-configuration.lisp"
    # cxml-dom
    "dom/package.lisp"
    "dom/dom-impl.lisp"
    "dom/dom-builder.lisp"
    "dom/dom-sax.lisp"
    # cxml-klacks
    "klacks/package.lisp"
    "klacks/klacks.lisp"
    "klacks/klacks-impl.lisp"
    "klacks/tap-source.lisp"
  ];

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
    depot.third_party.lisp.closure-common
    depot.third_party.lisp.puri
    depot.third_party.lisp.trivial-gray-streams
  ];

  tests = {
    name = "cxml-test";

    srcs = getSrcs [
      "test/domtest.lisp"
      "test/xmlconf.lisp"
    ];

    expression = ''
    ; Seems almost impossible to get DOM-Test-Suite to work today.
    (progn
      (xmlconf:run-all-tests 'xmlconf::sax-test "${xml-conformance-test-suite}/")
      (xmlconf:run-all-tests 'xmlconf::klacks-test "${xml-conformance-test-suite}/")
      t)
  '';
  };
}
