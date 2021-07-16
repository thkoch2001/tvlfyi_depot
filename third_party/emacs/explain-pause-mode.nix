{ pkgs, ... }:

let
  inherit (pkgs) emacsPackages fetchFromGitHub;

  commit = "35f7d780a9c164b5c502023746473b1de3857904";

in emacsPackages.melpaBuild rec {
  pname = "explain-pause-mode";
  version = "0.1"; # master on 20200603
  inherit commit;

  recipe = builtins.toFile "recipe.el" ''
    (explain-pause-mode :fetcher github
                        :repo "lastquestion/explain-pause-mode")
  '';

  src = fetchFromGitHub {
    owner = "lastquestion";
    repo = "explain-pause-mode";
    rev = commit;
    sha256 = "0d9lwzqqwmz0n94i7959rj7m24265yf3825a5g8cd7fyzxznl1pc";
  };
}
