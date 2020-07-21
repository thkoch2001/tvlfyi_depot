
{ depot, pkgs, ... }:

let

  src = fetchTarball {
    url = "http://beta.quicklisp.org/archive/fare-utils/2017-01-24/fare-utils-20170124-git.tgz";
    sha256 = "01wsr1aap3jdzhn4hrqjbhsjx6qci9dbd3gh4gayv1p49rbg8aqr";
  };

in depot.nix.buildLisp.library {
  name = "fare-utils";

  deps = with depot.third_party.lisp; [
    uiop
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "base/utils.lisp"
    "base/character-classes.lisp"
    "base/strings.lisp"
    "base/symbols.lisp"
    "base/macros.lisp"
    "base/lists.lisp"
    "base/packages.lisp"
    "base/objects.lisp"
    "base/streams.lisp"
    "base/hash-tables.lisp"
    "base/more-strings.lisp"
    "base/parse-cl-syntax.lisp"
    "filesystem/pathnames.lisp"
    "filesystem/files.lisp"
    "filesystem/atomic.lisp"
    "stateful/package.lisp"
    "stateful/container.lisp"
    "stateful/dllist.lisp"
  ];
}
