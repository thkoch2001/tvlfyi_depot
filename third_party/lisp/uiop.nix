{ depot, pkgs, ... }:

let

  src = fetchTarball {
    url = "http://beta.quicklisp.org/archive/uiop/2019-05-21/uiop-3.3.3.tgz";
    sha256 = "1vigla6pkys1kv8z2ydp7pgnrgjkqibfmjihkzvdqgdx8l2ilra6";
  };

in depot.nix.buildLisp.library {
  name = "uiop";
  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "common-lisp.lisp"
    "utility.lisp"
    "version.lisp"
    "os.lisp"
    "pathname.lisp"
    "filesystem.lisp"
    "stream.lisp"
    "image.lisp"
    "lisp-build.lisp"
    "launch-program.lisp"
    "run-program.lisp"
    "configuration.lisp"
    "backward-driver.lisp"
    "driver.lisp"
  ];
}
