{ depot, pkgs, ... }@args:

let
  inherit (import ../builder.nix args) buildGerritBazelPlugin;
in
buildGerritBazelPlugin rec {
  name = "code-owners";
  depsOutputHash = "sha256:07yg3klnjz9binnmzq86rb5w5sicfr01pqypjyfjz6jyhnfbb9m1";
  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/plugins/code-owners";
    rev = "6fdf3ce2e52904b35e2a5824a4197155c2c6b4e4";
    sha256 = "sha256:17k6310py71wax3881mf3vsf9zas648j4xzs9h0d7migv5nzsdzs";
  };
  patches = [
    ./using-usernames.patch
  ];
}
