# Common Lisp bindings to OpenSSL
{ depot, pkgs, ... }:

with depot.nix;

let
  src = pkgs.fetchgit {
    url = "https://github.com/cl-plus-ssl/cl-plus-ssl.git";
    rev = "29081992f6d7b4e3aa2c5eeece4cd92b745071f4";
    hash = "sha256:16lyrixl98b7vy29dbbzkbq0xaz789350dajrr1gdny5i55rkjq0";
  };
in
buildLisp.library {
  name = "cl-plus-ssl";
  deps = with depot.third_party.lisp; [
    alexandria
    bordeaux-threads
    cffi
    flexi-streams
    trivial-features
    trivial-garbage
    trivial-gray-streams
    {
      scbl = buildLisp.bundled "uiop";
      default = buildLisp.bundled "asdf";
    }
    { sbcl = buildLisp.bundled "sb-posix"; }
  ];

  native = [ pkgs.openssl ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "reload.lisp"
    "conditions.lisp"
    "ffi.lisp"
    "x509.lisp"
    "ffi-buffer-all.lisp"
    "ffi-buffer.lisp"
    "streams.lisp"
    "bio.lisp"
    "random.lisp"
    "context.lisp"
    "verify-hostname.lisp"
  ];

  brokenOn = [
    "ecl" # dynamic cffi
  ];
}
