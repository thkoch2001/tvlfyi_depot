# Hunchentoot is a web framework for Common Lisp.
{ depot, pkgs, ...}:

let
  src = with pkgs; srcOnly lispPackages.hunchentoot;

  url-rewrite = depot.nix.buildLisp.library {
    name = "url-rewrite";

    srcs = map (f: src + ("/url-rewrite/" + f)) [
      "packages.lisp"
      "specials.lisp"
      "primitives.lisp"
      "util.lisp"
      "url-rewrite.lisp"
    ];
  };
in depot.nix.buildLisp.library {
  name = "hunchentoot";

  deps = with depot.third_party.lisp; [
    alexandria
    bordeaux-threads
    chunga
    cl-base64
    cl-fad
    rfc2388
    cl-plus-ssl
    cl-ppcre
    flexi-streams
    md5
    trivial-backtrace
    usocket
    url-rewrite
  ];

  srcs = map (f: src + ("/" + f)) [
    "hunchentoot.asd"
    "packages.lisp"
    "compat.lisp"
    "specials.lisp"
    "conditions.lisp"
    "mime-types.lisp"
    "util.lisp"
    "log.lisp"
    "cookie.lisp"
    "reply.lisp"
    "request.lisp"
    "session.lisp"
    "misc.lisp"
    "headers.lisp"
    "set-timeouts.lisp"
    "taskmaster.lisp"
    "acceptor.lisp"
    "easy-handlers.lisp"
  ];

  brokenOn = [
    "ecl" # dynamic cffi
  ];
}
