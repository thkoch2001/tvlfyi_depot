{ depot, pkgs, lib, ... }:

let
  print-ast = depot.users.Profpatsch.writers.rustSimple {
    name = "print-ast";
    dependencies = with depot.users.Profpatsch.rust-crates; [
      libloading
      tree-sitter
    ];
    buildInputs = [
      pkgs.tree-sitter
    ];
  } ''
    extern crate libloading;
    extern crate tree_sitter;
    use std::mem;
    use std::io::{Read};
    use libloading::{Library, Symbol};
    use tree_sitter::{Language, Parser};

    /// Load the shared lib FILE and return the language under SYMBOL-NAME.
    /// Inspired by the rust source of emacs-tree-sitter.
    fn _load_language(file: String, symbol_name: String) -> Result<Language, libloading::Error> {
        let lib = Library::new(file)?;
        let tree_sitter_lang: Symbol<'_, unsafe extern "C" fn() -> _> =
            unsafe { lib.get(symbol_name.as_bytes())? };
        let language: Language = unsafe { tree_sitter_lang() };
        // Avoid segmentation fault by not unloading the lib, as language is a static piece of data.
        // TODO: Attach an Rc<Library> to Language instead.
        mem::forget(lib);
        Ok(language)
    }

    fn main() {
      let mut args = std::env::args();
      let so = args.nth(1).unwrap();
      let symbol_name = args.nth(0).unwrap();
      let file = args.nth(0).unwrap();
      let mut parser = Parser::new();
      let lang = _load_language(so, symbol_name).unwrap();
      parser.set_language(lang).unwrap();
      let mut bytes = Vec::new();
      let mut file = std::fs::OpenOptions::new().read(true).open(file).unwrap();
      file.read_to_end(&mut bytes);
      print!("{}", parser.parse(&bytes, None).unwrap().root_node().to_sexp());
    }


  '';

  tree-sitter-nix = buildTreeSitterGrammar {
    language = "tree-sitter-nix";
    source = pkgs.fetchFromGitHub {
      owner = "cstrahan";
      repo = "tree-sitter-nix";
      rev = "791b5ff0e4f0da358cbb941788b78d436a2ca621";
      sha256 = "1y5b3wh3fcmbgq8r2i97likzfp1zp02m58zacw5a1cjqs5raqz66";
    };
  };

  parse-nix-file = depot.nix.writeExecline "parse-nix-file" { readNArgs = 1; } [
    print-ast "${tree-sitter-nix}/parser" "tree_sitter_nix" "$1"
  ];

  # copied from nixpkgs
  buildTreeSitterGrammar =
      {
        # language name
        language
        # source for the language grammar
      , source
      }:

      pkgs.stdenv.mkDerivation {

        pname = "${language}-grammar";
        inherit (pkgs.tree-sitter) version;

        src = source;

        buildInputs = [ pkgs.tree-sitter ];

        dontUnpack = true;
        configurePhase= ":";
        buildPhase = ''
          runHook preBuild
          scanner_cc="$src/src/scanner.cc"
          if [ ! -f "$scanner_cc" ]; then
            scanner_cc=""
          fi
          $CXX -I$src/src/ -c $scanner_cc
          $CC -I$src/src/ -shared -o parser -Os  scanner.o $src/src/parser.c -lstdc++
          runHook postBuild
        '';
        installPhase = ''
          runHook preInstall
          mkdir $out
          mv parser $out/
          runHook postInstall
        '';
      };

in {
  inherit
    print-ast
    tree-sitter-nix
    parse-nix-file
    ;
}
