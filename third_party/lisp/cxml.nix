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

  catalogDtd = pkgs.runCommandNoCC "catalog.dtd" {} ''
    cp "${src}/catalog.dtd" "$out"
  '';

  catalogLisp = pkgs.runCommandNoCC "catalog.lisp" {} ''
    cp "${src}/xml/catalog.lisp" "$out"
    patch "$out" "${catalogPatch}"
  '';

  catalogPatch = pkgs.writeText "catalog-from-store.patch" ''
    --- catalog.lisp.orig	2021-07-27 22:37:45.707716035 +0200
    +++ catalog.lisp	2021-07-27 22:38:39.230570532 +0200
    @@ -226,9 +226,7 @@
           (warn "ignoring catalog error: ~A" c))))
     
     (defparameter *catalog-dtd*
    -    (let* ((cxml
    -            (slot-value (asdf:find-system :cxml) 'asdf::relative-pathname))
    -           (dtd (merge-pathnames "catalog.dtd" cxml)))
    +    (let ((dtd "${catalogDtd}"))
           (with-open-file (s dtd :element-type '(unsigned-byte 8))
             (let ((bytes
                    (make-array (file-length s) :element-type '(unsigned-byte 8))))
  '';
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
  ] ++ [
    catalogLisp
  ] ++ getSrcs [
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
