{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "sharplispers";
    repo = "parse-number";
    rev = "7707b224c4b941c2cbd28459113534242cee3a31";
    sha256 = "0sk06ib1bhqv9y39vwnnw44vmbc4b0kvqm37xxmkxd4dwchq82d7";
  };

in depot.nix.buildLisp.library {
  name = "parse-number";
  srcs = map (f: src + ("/" + f)) [ "parse-number.lisp" ];
}
