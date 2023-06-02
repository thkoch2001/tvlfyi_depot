{ pkgs, lib, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "ast-grep";
  version = "0.5.1";

  src = pkgs.fetchFromGitHub {
    owner = "ast-grep";
    repo = "ast-grep";
    rev = version;
    hash = "sha256:0gfjnbipckg78in3m0bgi7f2hyjq4m7k45kdnghlhdr7j8qf8kzv";
  };

  cargoHash = "sha256:009h0xyf81qxvm3kpxc8f2qddf1h8kgaf68c7lrhbnsndjzwzy9h";

  meta = with lib; {
    description = "A fast and polyglot tool for code searching, linting, rewriting at large scale";
    homepage = "https://ast-grep.github.io/";
    license = licenses.mit;
    maintainers = [ maintainers.tazjin ];
  };

  # Helper function to create a config file with tree-sitter grammars
  # for sg.
  #
  # Needs to be called with an attribute set mapping file extensions
  # to grammars (e.g. from nixpkgs.tree-sitter-grammars).
  # Unfortunately the file extension metadata does not exist in
  # nixpkgs, so it is required here. For example:
  #
  # ast-grep.configWithLanguages (with pkgs.tree-sitter-grammars; {
  #   "nix" = tree-sitter-nix;
  #   "lisp" = tree-sitter-commonlisp;
  #   "el" = tree-sitter-elisp;
  # })
  #
  # All derivation names should match the `tree-sitter-$lang-grammar`
  # package name, with `$lang` being used as the grammar name in
  # ast-grep. The config file must be passed to ast-grep in one of the
  # supported ways.
  passthru.configWithLanguages = tsmappings:
    let
      languages = builtins.listToAttrs (map
        (extension: {
          # parse the `tree-sitter-$lang-grammar` name:
          name =
            let parsed = builtins.match "^tree-sitter-(.*)-grammar" tsmappings.${extension}.pname;
            in if builtins.isNull parsed
            then throw "tree-sitter-grammar for ${extension} has incorrect package name"
            else builtins.head parsed;
          value = {
            extensions = [ extension ];
            libraryPath = "${tsmappings.${extension}}/parser";
          };
        })
        (builtins.attrNames tsmappings));
    in
    pkgs.writeText "sgconfig.yml" (builtins.toJSON {
      ruleDirs = [ ];
      customLanguages = languages;
    });
}
