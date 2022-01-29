{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "rwiker";
  repo = "trivial-ldap";
  rev = "3b8f1ff85f29ea63e6ab2d0d27029d68b046faf8";
  sha256 = "1zaa4wnk5y5ff211pkg6dl27j4pjwh56hq0246slxsdxv6kvp1z9";
};
in
depot.nix.buildLisp.library {
  name = "trivial-ldap";

  deps = with depot.third_party.lisp; [
    usocket
    cl-plus-ssl
    cl-yacc
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "trivial-ldap.lisp"
  ];

  brokenOn = [
    "ecl" # dynamic cffi
  ];
}
