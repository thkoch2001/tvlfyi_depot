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
}
