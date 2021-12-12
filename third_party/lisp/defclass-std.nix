# A shortcut macro to write DEFCLASS forms quickly
# Seems to be unmaintained (since early 2021)
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "EuAndreh";
    repo = "defclass-std";
    rev = "a4d32260a619eddf3a3e49df3af304f3c07ccec6";
    sha256 = "1c0ymb49wd205lzxmnmsrpqyv0pn61snn2xvsbk5iis135r4fr18";
  };
in depot.nix.buildLisp.library {
  name = "defclass-std";
  deps = with depot.third_party.lisp; [ alexandria anaphora ];

  srcs = map (f: src + ("/src/" + f)) [ "defclass-std.lisp" ];
}
