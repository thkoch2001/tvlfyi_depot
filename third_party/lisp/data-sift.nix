{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "archimag";
    repo = "data-sift";
    rev = "fd617d8200cdcc1b87ecf45ab59bb38e8b16ef7e";
    sha256 = "1v7gf0x4ibjzp0c56n9m77hxdgwcm9356zlk5n4l3fx4i0hj6146";
  };

in
depot.nix.buildLisp.library {
  name = "data-sift";
  deps = with depot.third_party.lisp; [
    cl-ppcre
    parse-number
    alexandria
    puri
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "packages.lisp"
    "conditions.lisp"
    "sift.lisp"
  ];

}
