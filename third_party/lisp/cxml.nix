{ depot, pkgs, lib, ... }:

let
  src = pkgs.fetchgit {
    url = "https://repo.or.cz/cxml.git";
    rev = "9365c4b93c3e5adc55a6512b3fb453693d06a707"; # 2011-06-08
    sha256 = "0sn3c3clndgj4xpwiqrrzkg7qrbqr38ig1cdhqb9s7bfhwgb8rdv";
  };

  getSrcs = builtins.map (p: "${src}/${p}");

  # cxml was tested against the 2008 version of the xml conformance test
  # suite, the 2013 version is missing some files it expects
  xml-conformance-test-suite = pkgs.fetchzip {
    url = "https://www.w3.org/XML/Test/xmlts20080827.tar";
    sha256 = "0r3ym07jwnqrd6rh4mb29wlmpf8spy82zi70gmz2vw34cl7jhwln";
  };
in

lib.fix (self:
  (depot.nix.buildLisp.library {
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
      ; Seems impossible to get DOM-Test-Suite to work today
      (progn
        (xmlconf:run-all-tests 'xmlconf::sax-test "${xml-conformance-test-suite}/")
        (xmlconf:run-all-tests 'xmlconf::klacks-test "${xml-conformance-test-suite}/")
        t)
    '';
    };
  }).overrideAttrs (drv: {
    buildCommand = ''
    cp "${src}/catalog.dtd" ./
  '' + drv.buildCommand;

    # HACK: update new drv.sbcl's self reference.
    passthru = drv.passthru // {
      sbcl = depot.nix.buildLisp.sbclWith [ self ];
    };
  })
)
