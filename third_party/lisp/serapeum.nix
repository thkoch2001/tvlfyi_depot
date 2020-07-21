# This is only packaging things from level 0 and 1, for now
{ depot, ... }:

let

  src = depot.third_party.fetchFromGitHub {
    owner = "ruricolist";
    repo = "serapeum";
    rev = "4a54a1760fc9c3806208c665f0a7fd6e22c9d8ab";
    sha256 = "1kxhsh84qv2b7vf9sgf50qpibcjkifc6h8nb73hnv8kxn14ns48k";
  };

in depot.nix.buildLisp.library {
  name = "serapeum";
  deps = with depot.third_party.lisp; [
    alexandria
    split-sequence
    parse-number
    named-readtables
    parse-declarations
    trivial-file-size
    trivia
    introspect-environment
    trivial-cltl2
    fare-quasiquote
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "macro-tools.lisp"
    "types.lisp"
    "iter.lisp"
    "definitions.lisp"
    "threads.lisp"
    "defining-types.lisp"
    "binding.lisp"
    "control-flow.lisp"
    "conditions.lisp"
    "op.lisp"
    "hash-tables.lisp"
    "functions.lisp"
    "trees.lisp"
    "files.lisp"
    "symbols.lisp"
    "arrays.lisp"
    "queue.lisp"
    "box.lisp"
    "numbers.lisp"
    "octets.lisp"
    "time.lisp"
    "clos.lisp"
    "hooks.lisp"
    "fbind.lisp"
    "reader.lisp"
    "packages.lisp"
    "heap.lisp"
    "lists.lisp"
    "sequences.lisp"
    "vectors.lisp"
    "exporting.lisp"
    "vector=.lisp"
    "mop.lisp"
    "internal-definitions.lisp"
    "tree-case.lisp"
    "dispatch-case.lisp"
    "range.lisp"
    "generalized-arrays.lisp"
    "units.lisp"
  ];
}
