{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.coreutils [ "head" "printf" "cat" ]
    // depot.nix.getBins pkgs.ncurses [ "tput" ]
    // depot.nix.getBins pkgs.bc [ "bc" ]
    // depot.nix.getBins pkgs.ocamlPackages.sexp [ "sexp" ];

  print-ast = depot.nix.writers.rustSimple
    {
      name = "print-ast";
      dependencies = with depot.third_party.rust-crates; [
        libloading
        tree-sitter
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
      let bytes = std::fs::read(&file).unwrap();
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

  watch-file-modified = depot.nix.writers.rustSimple
    {
      name = "watch-file-modified";
      dependencies = [
        depot.third_party.rust-crates.inotify
        depot.users.Profpatsch.netstring.rust-netstring
      ];
    } ''
    extern crate inotify;
    extern crate netstring;
    use inotify::{EventMask, WatchMask, Inotify};
    use std::io::Write;

    fn main() {
        let mut inotify = Inotify::init()
            .expect("Failed to initialize inotify");

        let file = std::env::args().nth(1).unwrap();

        let file_watch = inotify
            .add_watch(
                &file,
                WatchMask::MODIFY
            )
            .expect("Failed to add inotify watch");

        let mut buffer = [0u8; 4096];
        loop {
            let events = inotify
                .read_events_blocking(&mut buffer)
                .expect("Failed to read inotify events");

            for event in events {
                if event.wd == file_watch {
                  std::io::stdout().write(&netstring::to_netstring(file.as_bytes()));
                  std::io::stdout().flush();
                }
            }
        }
    }

  '';

  # clear screen and set LINES and COLUMNS to terminal height & width
  clear-screen = depot.nix.writeExecline "clear-screen" { } [
    "if"
    [ bins.tput "clear" ]
    "backtick"
    "-in"
    "LINES"
    [ bins.tput "lines" ]
    "backtick"
    "-in"
    "COLUMNS"
    [ bins.tput "cols" ]
    "$@"
  ];

  print-nix-file = depot.nix.writeExecline "print-nix-file" { readNArgs = 1; } [
    "pipeline"
    [ print-ast "${tree-sitter-nix}/parser" "tree_sitter_nix" "$1" ]
    "pipeline"
    [ bins.sexp "print" ]
    clear-screen
    "importas"
    "-ui"
    "lines"
    "LINES"
    "backtick"
    "-in"
    "ls"
    [
      "pipeline"
      # when you pull out bc to decrement an integer it’s time to switch to python lol
      [ bins.printf "x=%s; --x\n" "$lines" ]
      bins.bc
    ]
    "importas"
    "-ui"
    "l"
    "ls"
    bins.head
    "-n\${l}"
  ];

  print-nix-file-on-update = depot.nix.writeExecline "print-nix-file-on-update" { readNArgs = 1; } [
    "if"
    [ print-nix-file "$1" ]
    "pipeline"
    [ watch-file-modified "$1" ]
    "forstdin"
    "-d"
    ""
    "file"
    "importas"
    "file"
    "file"
    print-nix-file
    "$file"
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
      configurePhase = ":";
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

in
depot.nix.readTree.drvTargets {
  inherit
    print-ast
    tree-sitter-nix
    print-nix-file-on-update
    watch-file-modified
    ;
}
