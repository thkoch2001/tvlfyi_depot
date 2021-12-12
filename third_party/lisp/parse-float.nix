{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "soemraws";
    repo = "parse-float";
    rev = "3074765101e41222b6b624a66aaf1e6416379f9c";
    sha256 = "0jd2spawc3v8vzqf8ky4cngl45jm65fhkrdf20mf6dcbn3mzpkmr";
  };

in depot.nix.buildLisp.library {
  name = "parse-float";

  deps = with depot.third_party.lisp; [ alexandria ];

  srcs = map (f: src + ("/" + f)) [ "package.lisp" "parse-float.lisp" ];
}
